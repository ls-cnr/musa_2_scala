package org.icar.musa.scenarios.sps;

import org.icar.musa.pmr.*;

import java.util.ArrayList;

public class EvaluateSol {

    public EvaluateSol() {
    }

    public static ArrayList<String> solution_list(Solution sol) {

        ArrayList switchers = new ArrayList();
        int val;
        WfItem from = sol.start();
        WfFlow[] l = sol.arcs_out_from(from);
        while (!(l[0].to() instanceof WfEndEvent)) {
            WfTask to = (WfTask) l[0].to();
            String name_to = to.cap().name();
            String[] cap_name = name_to.split("_");
            val = cap_name.length;

            //stampo la stringa, vediamo la soluzione
            System.out.println(name_to);

            //controlla se è maggiore di 2, significa che c'è la &
            switch(val) {
                case 2:
                    //System.out.println(cap_name[0]);
                    if(!cap_name[1].equals("switchswauxg1") && !cap_name[1].equals("switchswauxg2") && !cap_name[1].equals("switchswmg1") && !cap_name[1].equals("switchswmg2"))
                    {
                        switchers.add(cap_name[1]);
                        if (cap_name[0].equals("CLOSE"))
                            switchers.add(1);
                        else switchers.add(0);
                    }
                    break;

                case 3:
                    //System.out.println(cap_name.length);
                    switchers.add(cap_name[0] + "sw"+cap_name[2]);
                    if (cap_name[1].equals("ON"))
                        switchers.add(1);
                    else switchers.add(0);
                    break;

                case 5:
                    //System.out.println(cap_name.length);
                    switchers.add(cap_name[1]);
                    if (cap_name[0].equals("CLOSE"))
                        switchers.add(1);
                    else switchers.add(0);
                    switchers.add(cap_name[4]);
                    if (cap_name[3].equals("CLOSE"))
                        switchers.add(1);
                    else switchers.add(0);
                    break;

            }

            l=sol.arcs_out_from(to);
        }
        return switchers;
    }
}



