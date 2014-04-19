package nef.nes.ce.java;

import clojure.lang.PersistentList;
import clojure.lang.PersistentVector;

import java.util.LinkedList;
import java.util.List;

public class Cell {
    protected LinkedList<Cell> out;
    protected LinkedList<Cell> in;
    protected LinkedList<Double> inWeights;
    protected int lr;
    protected double bias;
    protected int life;
    protected PersistentVector grammarRoot;
    protected Runnable activationFunction;
    protected int m_int;
    static protected Integer cnt=0;

    public Cell(LinkedList<Cell> out, LinkedList<Cell> in, LinkedList<Double> inWeights, int lr, double bias, int life, PersistentVector grammarRoot, Runnable activationFunction) {
        synchronized (cnt){
            m_int = cnt;
            cnt++;
        }
        this.out = out;
        this.in = in;
        this.inWeights = inWeights;
        this.lr = lr;
        this.bias = bias;
        this.life = life;
        this.grammarRoot = grammarRoot;
        this.activationFunction = activationFunction;        
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Cell clone(){
        return new Cell((LinkedList<Cell>)getOut().clone(),(LinkedList<Cell>)getIn().clone(), (LinkedList<Double>)getInWeights().clone(), lr, bias, life,
                        grammarRoot,activationFunction);
        
    }

    @Override
    public  boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        Cell cell = (Cell) o;
        if (m_int != cell.m_int) return false;
        return true;
    }

    @Override
    public int hashCode() {
        return m_int;
    }

    @Override
    public String toString() {
        return "Cell@"+m_int;
    }

    /*
     * returns new cell
     */
    public Cell handleSeq(PersistentVector left, PersistentVector right){
        Cell down = this.clone();
        down.setIn(new LinkedList<Cell>());
        down.getIn().add(this);
        down.setInWeights(new LinkedList<Double>());
        down.getInWeights().add(1.0);
        down.setGrammarRoot(right);
        down.setLr(0);
        setOut(new LinkedList<Cell>());
        getOut().add(down);
        setGrammarRoot(left);
        for (Cell c: down.out){
            c.getIn().set(c.getIn().indexOf(this), down);
        }
        return down;
    }

    /*
     * returns new cell
     */
    public Cell handlePar(PersistentVector left, PersistentVector right){
        Cell neighbor = this.clone();
        this.grammarRoot = left;
        neighbor.grammarRoot = right;
        for (Cell c: getIn()){
            c.getOut().add(c.getOut().indexOf(this)+1,neighbor);
        }
        for (Cell c: getOut()){
            int i = 1+c.getIn().indexOf(this);
            c.getIn().add(i, neighbor);
            c.getInWeights().add(i,c.getInWeights().get(i-1));
                
        }
        return neighbor;    
    }
    
    

    public LinkedList<Cell> getOut() {
        return out;
    }

    public void setOut(LinkedList<Cell> out) {
        this.out = out;
    }

    public LinkedList<Cell> getIn() {
        return in;
    }

    public void setIn(LinkedList<Cell> in) {
        this.in = in;
    }

    public LinkedList<Double> getInWeights() {
        return inWeights;
    }

    public void setInWeights(LinkedList<Double> inWeights) {
        this.inWeights = inWeights;
    }

    public int getLr() {
        return lr;
    }

    public void setLr(int lr) {
        this.lr = lr;
    }

    public double getBias() {
        return bias;
    }

    public double bias(){
        return bias;
    }

    public void setBias(double bias) {
        this.bias = bias;
    }

    public int getLife() {
        return life;
    }

    public void setLife(int life) {
        this.life = life;
    }

    public PersistentVector getGrammarRoot() {
        return grammarRoot;
    }

    public void setGrammarRoot(PersistentVector grammarRoot) {
        this.grammarRoot = grammarRoot;
    }

    public Runnable getActivationFunction() {
        return activationFunction;
    }

    public void setActivationFunction(Runnable activationFunction) {
        this.activationFunction = activationFunction;
    }

    public void setCurrentInLinkWeight(double w){
        this.inWeights.set(lr, w);
    }

    public void addCurrentInLinkWeight(double w){
        getInWeights().set(lr, getInWeights().get(lr) + w);
    }

    public void multiplyCurrentInLinkWeight(double w){
         getInWeights().set(lr, getInWeights().get(lr) * w);
    }

    public void cutCurrentInLink(){ //TODO
        if ((in.size() > 1) &&  (in.get (lr).out.size () > 1)){
            in.get(lr).out.remove(this);
            in.remove(lr);
            inWeights.remove(lr);
            lr = 0;
        }
    }

    public void decLife(){
        life--;
    }

    public void inclr(){
        lr++;
        if (lr >= in.size()) {
            lr = lr % in.size();
        }
    }

    public void declr(){
        lr--;
        if (lr<0) {
            lr = (in.size()-1 < 0)? 0 : in.size()-1;
        }
    }

}

