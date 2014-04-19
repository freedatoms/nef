package nef.nes.task.maze;


public enum MoveAction {
    FORWARD,TURN_LEFT, TURN_RIGHT;
    public static MoveAction getActionForInt(int m){
         switch (m) {
             case 0: return FORWARD;
             case 1: return TURN_LEFT;
             case 2: return TURN_RIGHT;
         }
        return  FORWARD;
    }
}
