package nef.nes.es.java;

import nef.nes.task.maze.DiscreteMaze;
import nef.nes.task.maze.DiscreteMazeViewer;

import javax.swing.*;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Random;
import clojure.lang.IFn;
import java.lang.Math;

public class Evolution {
    protected int layers[];
    protected FitnessFunction fitnessFunction;
    protected Random rand;

    public Evolution(int[] layers, FitnessFunction fitnessFunction) {
        this.fitnessFunction = fitnessFunction;
        this.layers = layers;
        this.rand = new Random();
    }

    private double getAvgPerformance(Individual[] pop){
	double sum = 0.0;
	for (int i = 0; i < pop.length; i++){
	    sum += pop[i].getSuccessRate();
	}
	return sum/pop.length;
    }

    protected Individual[] marriage(Individual[] pop,int rho /*Family size*/){
        Individual family[] = new Individual [rho];
        for (int i = 0; i < rho; i++) {
            family[i]=pop[rand.nextInt(pop.length)];
        }
        return family;
    }

    protected Individual[] commaSelection(Individual[] inds, int mu){
        if (mu > inds.length){
            throw new Error("Comma selection requires lambda > mu.");
        }

        Individual res[] = new Individual[mu];
        Arrays.sort(inds, new Comparator<Individual>() {
            @Override
            public int compare(Individual o1, Individual o2) {
                if (o1.getFitness() > o2.getFitness()){
                    return -1;
                } else if (o1.getFitness()==o2.getFitness()){
                    return 0;
                } else {
                    return 1;
                }
            }
        });
        System.arraycopy(inds,0,res,0,mu);
        return res;
    }

    protected Individual[] plusSelection(Individual[] inds, Individual[] newInds, int mu){
        Individual res[] = new Individual[mu];
        Individual _inds[] = new Individual[inds.length+newInds.length];
        System.arraycopy(inds,0,_inds,0,inds.length);
        System.arraycopy(newInds,0,_inds,inds.length,newInds.length);
        Arrays.sort(_inds, new Comparator<Individual>() {
            @Override
            public int compare(Individual o1, Individual o2) {
                if (o1.getFitness() > o2.getFitness()){
                    return -1;
                } else if (o1.getFitness()==o2.getFitness()){
                    return 0;
                } else {
                    return 1;
                }
            }
        });
        System.arraycopy(_inds,0,res,0,mu);
        return res;
    }

    protected double [] s_recombination(Individual[] family){
        int len = family[0].std.length;
        double variance[] = new double[len];

        for (int j = 0; j < family.length; j++) {
            double vars[] = family[j].std;
            for (int i = 0; i < len; i++) {
                variance[i] += vars[i];
            }
        }
        // otherwise it could give wrong result due to non-commutative nature of fp arithmetics
        for (int i = 0; i < len; i++) {
            variance[i] /= family.length;
        }
        return variance;
    }

    protected double [] s_mutation(double[] genome){
        double c = 1;
        double N = genome.length;
        double tau=c/Math.sqrt(2*N);
        double tau0 = c/Math.sqrt(2*Math.sqrt(N));

        for (int i = 0; i < N; i++) {
            genome[i] = Math.exp(tau0*rand.nextGaussian())
                    *(genome[i]*Math.exp(tau*rand.nextGaussian()));
        }

        return genome;
    }

    protected double [] y_recombination(Individual[] family){      // Intermediate rho recombination
        int len = family[0].net.getWeights().length;
        double genome[] = new double[len];

        for (int j = 0; j < family.length; j++) {
            double weights[] = family[j].net.getWeights();
            for (int i = 0; i < len; i++) {
                genome[i] += weights[i];
            }
        }
        // otherwise it could give wrong result due to non-commutative nature of fp arithmetics
        for (int i = 0; i < len; i++) {
            genome[i] /= family.length;
        }
        return genome;
    }

    protected double [] y_mutation(double[] genome, double [] mutStrength){

        for (int i = 0; i < genome.length; i++) {
            genome[i] += rand.nextGaussian()*mutStrength[i];
        }
        return genome;
    }

    public double[] evolve(int generations, int mu, int rho, int lambda, boolean commaSelectionp, Logger log, IFn bestIndFunc){
        Individual inds []= new Individual[mu];
        Individual newInds [] = new Individual[lambda];

        for (int i = 0; i<mu;i++){
            inds[i] = new Individual(layers,this);
        }
	double maxA [] = new double[generations];
        double max = -Double.MAX_VALUE;
        Individual family[][] = new Individual[lambda][];
        for (int i = 0; i < generations; i++){
            //double mutstr[] = s_recombination(inds); //Intermediate (mu, mu) recombination
            for (int l = 0; l < lambda; l++) {
                family[l] = marriage(inds, rho);
                double sf[] = s_mutation(s_recombination(family[l]));
                //double sf[] = s_mutation(mutstr);    //Intermediate (mu, mu) recombination
                newInds[l]=new Individual(layers,y_mutation(y_recombination(family[l]),sf),sf,this);
            }

            inds=(commaSelectionp)? commaSelection(newInds,mu) : plusSelection(inds,newInds,mu);

            max = Math.max(max, inds[0].getSuccessRate());
	    maxA[i] = max;
	    bestIndFunc.invoke(inds[0], i, inds[0].getSuccessRate());
	    double median=((inds[(int)(Math.floor(mu/2.0))].getSuccessRate() + 
			    inds[(int)(Math.ceil(mu/2.0))].getSuccessRate()) / 2.0);
	    log.log(i,
		    inds[0].getSuccessRate(),
		    inds[mu-1].getSuccessRate(),
		    getAvgPerformance(inds),
		    median,
		    max);
            System.out.printf("Generation %5d\tbest: %1.4f\tworst: %1.4f\tmax success rate: %1.4f\n",i,inds[0].getFitness(),inds[mu-1].getFitness(),max);
        }
        return maxA;
    }


    // public double[] evolveMaze(DiscreteMaze maze, String filename, int generations, int mu, int rho, int lambda, boolean commaSelectionp){
    //     Individual inds []= new Individual[mu];
    //     Individual newInds [] = new Individual[lambda];

    //     JFrame fr = new JFrame("Maze");
    //     fr.setSize(600,600);
    //     for (int i = 0; i<mu;i++){
    //         inds[i] = new Individual(layers,this);
    //     }

    // 	double maxA[] = new double[generations];
    //     double max = -Double.MAX_VALUE;
    //     Individual family[][] = new Individual[lambda][];
    //     for (int i = 0; i < generations; i++){
    //         //double mutstr[] = s_recombination(inds); //Intermediate (mu, mu) recombination
    //         for (int l = 0; l < lambda; l++) {
    //             family[l] = marriage(inds, rho);
    //             double sf[] = s_mutation(s_recombination(family[l]));
    //             //double sf[] = s_mutation(mutstr);    //Intermediate (mu, mu) recombination
    //             newInds[l]=new Individual(layers,y_mutation(y_recombination(family[l]),sf),sf,this);
    //         }

    //         inds=(commaSelectionp)? commaSelection(newInds,mu) : plusSelection(inds,newInds,mu);

    //         max = Math.max(max, inds[0].getSuccessRate());
    // 	    maxA[i] = max;
    //         fr.getContentPane().removeAll();
    // 	    DiscreteMazeViewer dmv = new DiscreteMazeViewer(maze, inds[0]);
    //         fr.getContentPane().add(dmv);
    // 	    dmv.saveImage(filename+"-"+i+".png", 800, 800);
    //         fr.setTitle("Maze Generation: "+i+" Fitness: "+inds[0].getFitness());
    // 	    fr.revalidate();
    // 	    if (!fr.isShowing()){
    // 		fr.setVisible(true);
    // 	    }
    //         System.out.printf("Generation %5d\tbest: %1.4f\tworst: %1.4f\tmax success rate: %1.4f\n",i,inds[0].getFitness(),inds[mu-1].getFitness(),max);
    //     }
    //     return maxA;
    // }


    public double evolveOneFifthRule(int generations, int G, double a){
        Individual inds []= new Individual[2];
        inds[0] = new Individual(layers,this);
        int Gs = 0;
        double Ps = 0;
        double sigma = 1;
        for (int i = 0; i < generations; i++) {
            if (i % G == G-1){
                Ps = ((double)Gs)/((double)G);
                if (Ps > 1.0/5){
                    sigma /= a;
                } else if (Ps < 1.0/5) {
                    sigma *= a;
                }
                Gs = 0;
            }

            inds[1] = inds[0].clone();
            inds[1].simpleMutation(sigma);
            if(inds[0].getFitness()<inds[1].getFitness()){
                inds[0] = inds[1];
                Gs++;
            }
            System.out.printf("Generation %5d\tfitness: %1.4f\tsigma: %1.4f\tPs: %1.4f\n",i,inds[0].getFitness(),sigma,Ps);
        }
        return inds[0].getFitness();
    }



    public FitnessFunction getFitnessFunction() {
        return fitnessFunction;
    }


}
