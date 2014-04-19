package nef.nes.task.maze;


public class XYNode implements Comparable {
    int distance;
    int x;
    int y;

    public XYNode(int x, int y, int distance) {
        this.distance = distance;
        this.x = x;
        this.y = y;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        XYNode xyNode = (XYNode) o;

        if (x != xyNode.x) return false;
        if (y != xyNode.y) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        return result;
    }

    public int getDistance() {
        return distance;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public void updateDistance(int i) {
        if (i<distance)
            distance=i;
    }

    @Override
    public int compareTo(Object o) {
        if (o instanceof XYNode)
           return distance - ((XYNode)o).distance;
        return -1;
    }
}
