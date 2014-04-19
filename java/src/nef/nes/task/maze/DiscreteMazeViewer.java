package nef.nes.task.maze;

import clojure.lang.AFn;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class DiscreteMazeViewer extends JPanel {
    DiscreteMaze dmaze;
    List<MoveAction> actions;

    public DiscreteMazeViewer(DiscreteMaze dmaze, AFn i) {
        this.dmaze = dmaze;
        actions = new LinkedList<>();
        Actor a = dmaze.getNewActor();
        int sp = dmaze.shortestPathToTarget(a.getX(),a.getY());
        for (int j = 0; j < sp * 10; j++) {
            MoveAction ma = getMoveAction((double[])i.invoke(new Double[]{(double)j,
                                                                          ((double)dmaze.shortestPathToTarget(a.getX(),a.getY())),
                                                                          dmaze.lookForward(a)=='w'?0.0:1.0,
                                                                          dmaze.lookLeft(a)=='w'?0.0:1.0,
                                                                          dmaze.lookRight(a)=='w'?0.0:1.0
                    }));
            actions.add(ma);
            if (dmaze.move(a,ma)){
                break;
            }
        }
    }

    private MoveAction getMoveAction(double []outs){
        double max=0;
        final MoveAction actions[] = {MoveAction.FORWARD,MoveAction.TURN_LEFT,MoveAction.TURN_RIGHT};
        int maxpos=0;
        for (int i = 0; i <outs.length; i++){
            if (max<outs[i]){
                maxpos = i;
                max = outs[i];
            }
        }
        if (maxpos > 2)
            return actions[0];
        return actions[maxpos];
    }

    private void doDrawing(Graphics g, List<MoveAction> Actions, int w, int h) {
        Graphics2D g2d = (Graphics2D) g;
        g2d.setColor(Color.white);
        g2d.fillRect(0,0,w,h);
        for (int i = 0; i < dmaze.getWidth(); i++) {
            for (int j = 0; j < dmaze.getHeight(); j++) {
                switch (dmaze.getTile(i,j)){
                    case 'w':
                        g2d.setColor(Color.black);
                        g2d.fillRect(i * w / dmaze.getWidth() ,j * h / dmaze.getHeight(),
                                w /  dmaze.getWidth()+1 , h / dmaze.getHeight() +1);
                    break;
                    case 's':
                        g2d.setColor(new Color(100,255,0));
                        g2d.fillRect(i * w /  dmaze.getWidth(), j * h / dmaze.getHeight(),
                                w /  dmaze.getWidth() +1, h / dmaze.getHeight() +1);
                    break;
                    case 't':
                        g2d.setColor(new Color(0,100,255));
                        g2d.fillRect(i * w /  dmaze.getWidth(),j * h / dmaze.getHeight(),
                                w /  dmaze.getWidth() +1, h / dmaze.getHeight() +1);
                    break;
                }
            }
        }
        g2d.setColor(Color.gray);
        for (int i = 0; i < dmaze.getWidth(); i++) {
            g2d.drawLine(i * w /  dmaze.getWidth(), 0, i * w / dmaze.getWidth(), h);
        }
        for (int j = 0; j < dmaze.getHeight(); j++) {
            g2d.drawLine(0, j * h /  dmaze.getHeight(), w, j * h / dmaze.getHeight());
        }

        g2d.setColor(new Color(255,155,0));
        g2d.setStroke(new BasicStroke(4,BasicStroke.CAP_ROUND,BasicStroke.JOIN_BEVEL));
        Actor a = dmaze.getNewActor();
        for (MoveAction ma:Actions){
            int xo = a.getX();
            int yo = a.getY();
            dmaze.move(a, ma);

            if (ma == MoveAction.FORWARD){
                g2d.drawLine((xo*w+w/2)/dmaze.getWidth(),(yo*h+h/2)/dmaze.getHeight(),
                        (a.getX()*w+w/2)/dmaze.getWidth(),(a.getY()*h+h/2)/dmaze.getHeight());
            }
        }
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Dimension size = getSize();
        Insets insets = getInsets();
        int w = size.width - insets.left - insets.right;
        int h = size.height - insets.top - insets.bottom;
        doDrawing(g,actions,w,h);
    }

    public void saveImage(String fileName,int w, int h){
        BufferedImage bi = new BufferedImage(w,h, BufferedImage.TYPE_INT_RGB);
        doDrawing(bi.createGraphics(),actions, w,h);
        try {
            ImageIO.write(bi, "png", new File(fileName));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
