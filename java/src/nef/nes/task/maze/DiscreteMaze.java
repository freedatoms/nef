package nef.nes.task.maze;

import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;

public class DiscreteMaze {
    protected char maze [][] = {
            {'w', 'w', 'w', 'w', 'w','w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w'},
            {'w', 's', ' ', ' ', ' ',' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w'},
            {'w', 'w', 'w', 'w', 'w','w', 'w', 'w', 'w', ' ', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ','w', ' ', ' ', ' ', ' ', 'w', ' ', 'w', ' ', ' ', ' ', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ','w', ' ', ' ', ' ', ' ', ' ', ' ', 'w', ' ', ' ', ' ', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ','w', ' ', ' ', ' ', ' ', ' ', ' ', 'w', ' ', 'w', ' ', 'w', 'w', 'w', 'w'},
            {'w', ' ', ' ', ' ', ' ','w', ' ', 'w', ' ', ' ', ' ', ' ', 'w', ' ', 'w', ' ', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ','w', ' ', 'w', 'w', ' ', ' ', ' ', 'w', ' ', 'w', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ','w', ' ', 'w', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', 'w', ' ', ' ', 'w', ' ', 'w', ' ', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', 'w', ' ', 'w', 'w', 'w', 'w', 'w', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', 'w', 'w', ' ', 'w', ' ', 'w', ' ', ' ', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', 'w', ' ', 'w', ' ', 'w', ' ', ' ', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', 'w', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', 'w', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', 'w', ' ', ' ', 't', ' ', ' ', ' ', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w'},
            {'w', ' ', ' ', ' ', ' ',' ', ' ', ' ', ' ', ' ', 'w', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'w'},
            {'w', 'w', 'w', 'w', 'w','w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w'}
    };
    protected int sx,sy,tx,ty;

    public DiscreteMaze() {
        initialize();
    }

    protected void initialize(){
        for (int i = 0; i < maze[0].length; i++) {
            for (int j = 0; j < maze.length; j++) {
                if (maze[j][i] == 's'){
                    sx = i;
                    sy = j;
                    if (tx+ty >0) return;
                }
                if (maze[j][i] == 't'){
                    tx = i;
                    ty = j;
                    if (sx+sy >0) return;
                }
            }
        }
    }

    public int manhattanDistance(int fromx, int fromy){
        return Math.abs(tx-fromx) + Math.abs(ty-fromy);
    }

    protected List<XYNode> getNeighbours(XYNode node){
        List<XYNode> lst = new LinkedList<>();
        int x = node.getX();
        int y = node.getY();
        if (getTile(x+1,y) != 'w'){
            lst.add(new XYNode(x+1,y,node.getDistance()+1));
        }
        if ((x>0) && (getTile(x-1,y) != 'w')){
            lst.add(new XYNode(x-1,y,node.getDistance()+1));
        }
        if (getTile(x,y+1) != 'w'){
            lst.add(new XYNode(x,y+1,node.getDistance()+1));
        }
        if ((y>0)&&(getTile(x,y-1) != 'w')){
            lst.add(new XYNode(x,y-1,node.getDistance()+1));
        }
        return lst;
    }

    public int shortestPath(int fromx, int fromy, int tox,int toy){
        if (getTile(fromx,fromy) == 'w')
            return Integer.MAX_VALUE;
        PriorityQueue<XYNode> pq = new PriorityQueue<>();
        pq.add(new XYNode(fromx,fromy, 0));
        int d[][]= new int[getWidth()][getHeight()];
        for (int i = 0; i < getWidth(); i++) {
            for (int j = 0; j < getHeight(); j++) {
                d[i][j]=Integer.MAX_VALUE;
            }
        }

        d[fromx][fromy]=0;
        while (!pq.isEmpty()){
            XYNode node = pq.poll();
            node.updateDistance(d[node.getX()][node.getY()]);
            for (XYNode n: getNeighbours(node)){
                if  (d[n.getX()][n.getY()]>n.getDistance()){
                    d[n.getX()][n.getY()] = n.getDistance();
                    pq.add(n);
                }
            }
        }
        return d[tox][toy];
    }

    public int shortestPathToTarget(int fromx, int fromy){
        if (getTile(fromx,fromy) == 'w')
            return Integer.MAX_VALUE;
        return shortestPath(fromx,fromy, tx,ty);
    }

    public boolean move(Actor a,MoveAction ma){
        switch (ma){
            case FORWARD:
                if (maze[a.getY()+a.getDy()][a.getX()+a.getDx()] == 'w'){
                    break;
                }
                a.forward();
                break;
            case TURN_RIGHT:
                a.turnRight();
                break;
            case TURN_LEFT:
                a.turnLeft();
                break;
        }
        return getTile(a.getX(),a.getY()) == 't';
    }

    public boolean isTarget(Actor a){
        return getTile(a.getX(),a.getY()) == 't';
    }

    public int getWidth(){
        return maze[0].length;
    }

    public int getHeight(){
        return maze.length;
    }

    public char getTile(int x, int y){
        if(x < 0 || x >= maze[0].length || y < 0 || y >= maze.length){
            return 'w';
        }
        return maze[y][x];
    }

    public Actor getNewActor() {
        return new Actor(sx,sy);
    }

    public char lookForward(Actor a){
        Actor tmp = a.clone();
        tmp.forward();
        return getTile(tmp.getX(),tmp.getY());
    }

    public char lookLeft(Actor a){
        Actor tmp = a.clone();
        tmp.turnLeft();
        tmp.forward();
        return getTile(tmp.getX(),tmp.getY());
    }

    public char lookRight(Actor a){
        Actor tmp = a.clone();
        tmp.turnRight();
        tmp.forward();
        return getTile(tmp.getX(),tmp.getY());
    }
}
