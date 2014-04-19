package nef.nes.task.maze;

public class Actor {
    protected int x;
    protected int y;
    protected int dx;
    protected int dy;

    public Actor(int x, int y) {
        this.x = x;
        this.y = y;
        dx = 1;
        dy = 0;
    }

    public Actor(int x, int y, int dx, int dy) {
        this.x = x;
        this.y = y;
        this.dx = dx;
        this.dy = dy;
    }

    public Actor clone(){
        return new Actor(getX(),getY(),getDx(),getDy());
    }

    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }

    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }

    public int getDx() {
        return dx;
    }

    public void setDx(int dx) {
        this.dx = dx;
    }

    public int getDy() {
        return dy;
    }

    public void setDy(int dy) {
        this.dy = dy;
    }

    public void forward(){
        x+=dx;
        y+=dy;
    }

    public void turnLeft(){
        int tm = dx;
        dx = (dx!=0)? 0 : dy;
        dy = (dy!=0)? 0 : -tm;
    }

    public void turnRight(){
        int tmp = dx;
        dx = (dx!=0)? 0 : -dy;
        dy = (dy!=0)? 0 : tmp;
    }
}
