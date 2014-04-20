package nef.nes.es.java;

public interface Logger{
    public void log(int generation, double maxPerf, double minPerf, double avgPerf,  double medianPerf, double maxPerfSinceBeg);
}
