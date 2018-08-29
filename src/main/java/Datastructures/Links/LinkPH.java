package Datastructures.Links;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class LinkPH {
    public ArrayList<LinkMultiple> links = new ArrayList<>();

    public void addLink(LinkMultiple link) {
        links.add(link);
    }

    public void removeLink(LinkMultiple link) {
        links.remove(link);
    }

    public String toString() {
        StringBuffer st = new StringBuffer();
        int size = links.get(0).size();
        for(int i = 0; i < size; ++i) {
            for(LinkMultiple linkm : links) {st.append(linkm.literals.get(i).toString());}
            st.append("\n");}
        return st.toString();
    }

}
