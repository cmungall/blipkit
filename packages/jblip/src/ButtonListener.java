import jpl.*;
import java.awt.*;
import java.awt.event.*;

/** A listener that you attach to the top-level Frame or JFrame of
 *  your application, so quitting the frame exits the application.
 *  1998-99 Marty Hall, http://www.apl.jhu.edu/~hall/java/
 */

public class ButtonListener implements ActionListener {
    public int num;
    public Object obj;
    public Term term;
    public void setNum(int n) {
        num=n;
    }
    public void setObj(Object o) {
        obj=o;
    }
    public void setTerm(Term t) {
        term=t;
    }

    public void actionPerformed(ActionEvent e) {
        //numClicks++;
        //label.setText(labelPrefix + numClicks);
        System.out.println("num= " + num);
        //System.out.println("obj= " + obj);
        //System.out.println("term= " + term);
        //System.out.println("OI " + e);
        Query qx = new Query("action_callback("+num+")");
        System.out.println( "x " + (qx.hasSolution() ? "succeeded" : "failed") );      

    }

}   
