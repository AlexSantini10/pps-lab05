package it.unibo.pps.polyglot.a01a;

import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.awt.event.ActionListener;
import it.unibo.pps.polyglot.Pair;
public class GUI extends JFrame {

    private static final long serialVersionUID = -6218820567019985015L;
    private final Map<JButton,Pair<Integer,Integer>> buttons = new HashMap<>();
    private final Logics logics;

    public GUI(int size, int boat) {
        this.logics = new LogicsImpl(size,boat);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setSize(100*size, 100*size);

        JPanel panel = new JPanel(new GridLayout(size,size));
        this.getContentPane().add(BorderLayout.CENTER,panel);

        ActionListener al = (e)->{
            final JButton bt = (JButton)e.getSource();
            final Pair<Integer,Integer> p = buttons.get(bt);
            //System.out.println("hit "+p);
            final Result result = logics.hit(p.getY(), p.getX());
            final boolean isHit = result == Result.HIT || result == Result.WON;
            bt.setText(isHit ? "X" : "O");
            bt.setEnabled(false);
            if (result==Result.WON || result==Result.LOST){
                System.out.println(result.name());
                System.exit(0);
            }
        };

        for (int i=0; i<size; i++){
            for (int j=0; j<size; j++){
                final JButton jb = new JButton(" ");
                jb.addActionListener(al);
                this.buttons.put(jb,new Pair<>(j,i));
                panel.add(jb);
            }
        }
        this.setVisible(true);
    }

}
