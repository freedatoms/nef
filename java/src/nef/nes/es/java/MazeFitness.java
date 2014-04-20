package nef.nes.es.java;

import nef.nes.task.maze.Actor;
import nef.nes.task.maze.DiscreteMaze;
import nef.nes.task.maze.MoveAction;


public class MazeFitness extends FitnessFunction {
    protected DiscreteMaze maze;
    protected boolean norm;
    public MazeFitness(DiscreteMaze maze, boolean normalize) {
        this.maze = maze;
	this.norm = normalize;
    }

    private MoveAction getMoveAction(double []outs){
        double max=0;
        final MoveAction actions[] = {MoveAction.FORWARD,MoveAction.TURN_LEFT,MoveAction.TURN_RIGHT};
        int maxpos=0;
        for (int i = 0; i < 3; i++){
            if (max<outs[i]){
                maxpos = i;
                max = outs[i];
            }
        }
        return actions[maxpos];
    }

    @Override
    public double countFitness(Individual i) {
        if (norm){
	    return countFitnessUsingNormInputs(i);
	}
	Actor a = maze.getNewActor();
        int sp = maze.shortestPathToTarget(a.getX(),a.getY());
        for (int j = 0; j < sp * 10; j++) {
            MoveAction ma = getMoveAction(i.evaluate(new double[]{(double)j,
								  ((double)maze.shortestPathToTarget(a.getX(),a.getY())),
								  maze.lookForward(a)=='w'?0:1,
								  maze.lookLeft(a)=='w'?0:1,
								  maze.lookRight(a)=='w'?0:1
                    }));

	    if (maze.move(a,ma)){
		return 100 + (double)sp/(double)j;
	    }
        }
        return 100 - maze.shortestPathToTarget(a.getX(),a.getY());
    }

    protected double countFitnessUsingNormInputs(Individual i){
	Actor a = maze.getNewActor();
        int sp = maze.shortestPathToTarget(a.getX(),a.getY());
        for (int j = 0; j < sp * 10; j++) {
            MoveAction ma = getMoveAction(i.evaluate(new double[]{(double)j/((double)(10*sp)),
								  ((double)maze.shortestPathToTarget(a.getX(),a.getY()))/(double)sp,
								  maze.lookForward(a)=='w'?0:1,
								  maze.lookLeft(a)=='w'?0:1,
								  maze.lookRight(a)=='w'?0:1
                    }));

	    if (maze.move(a,ma)){
		return 100 + (double)sp/(double)j;
	    }
        }
        return 100 - maze.shortestPathToTarget(a.getX(),a.getY());
    }
}

