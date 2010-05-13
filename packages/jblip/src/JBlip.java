import jpl.*;
import jpl.Query;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


public class JBlip implements ActionListener { 
    public static void
        main(String argv[]) {

        Query qx = new Query("consult('jblip.pro')");
        System.out.println( "x " + (qx.hasSolution() ? "succeeded" : "failed") );      
        Query qy = new Query("prolog");
        System.out.println( "x " + (qy.hasSolution() ? "succeeded" : "failed") );      
    }

    public void actionPerformed(ActionEvent e) {
        //numClicks++;
        //label.setText(labelPrefix + numClicks);
        System.out.println("ACTION " + e);
    }

}

