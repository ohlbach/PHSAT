package Datastructures.Literals;

import Datastructures.Links.LinkMultiple;
import Datastructures.Links.LinkPH;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class PHLiteral extends CLiteral {
    ArrayList<LinkMultiple> linksMultiple = new ArrayList<>();
    ArrayList<LinkPH> linksPH = new ArrayList<>();

    public PHLiteral(int literal) {
        super(literal);
    }


}
