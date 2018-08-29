package Datastructures.Literals;

import Datastructures.Links.LinkBinary;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class CGLiteral extends CLiteral {

    ArrayList<LinkBinary> links;

    public CGLiteral(int literal) {
        super(literal);
    }

    public void addLink(LinkBinary link) {
        links.add(link);
    }

    public void removeLink(LinkBinary link) {
        links.remove(link);
    }
}
