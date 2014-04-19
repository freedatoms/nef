package nef.nes.es.java;

public class RegressionFitness extends FitnessFunction{
    protected double in[][];
    protected double out[];

    public RegressionFitness(double[][] In, double[] Out) {
        this.in = In;
        this.out = Out;
    }

    @Override
    public double countFitness(Individual ind) {
        double cnt=0;
        for (int i = 0; i < in.length;i++){
            cnt -= Math.pow(((out[i] - ind.evaluate(in[i])[0])),2);
        }
        return (cnt+in.length)/in.length;
    }

    @Override
    public double countSuccessRate(Individual ind) {
        double cnt=0;
        for (int i = 0; i < in.length;i++){
            cnt -= Math.abs(((out[i] - ind.evaluate(in[i])[0])));
        }
        return (cnt+in.length)/in.length;
    }
}







