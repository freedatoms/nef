package nef.nes.es.java;

public abstract class FitnessFunction {
    public abstract double countFitness(Individual i);
    public double countSuccessRate(Individual i){
	return countFitness(i);
    }
}
