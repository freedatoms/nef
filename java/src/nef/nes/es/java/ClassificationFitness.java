package nef.nes.es.java;

public class ClassificationFitness extends FitnessFunction{
    protected double in[][];
    protected int out[];

    public ClassificationFitness(double[][] in, int[] out) {
        this.in = in;
        this.out = out;
    }

    private int maxPos(double []outs){
        double max=0;
        int maxpos=0;
        for (int i = 0; i < outs.length; i++){
            if (max<outs[i]){
                maxpos = i;
                max = outs[i];
            }
        }
        return maxpos;
    }

    @Override
    public double countFitness(Individual ind) {
        double cnt=0;
        for (int i = 0; i < in.length;i++){
            cnt += (out[i] == maxPos(ind.evaluate(in[i]))) ? 1: 0;
        }
        return cnt/in.length;
    }
}
