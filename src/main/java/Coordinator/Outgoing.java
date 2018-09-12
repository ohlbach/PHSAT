package Coordinator;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 12.09.2018.
 */
public class Outgoing {
    private ArrayList<Consumer> observers = new ArrayList<>();

    public synchronized void addObserver(Consumer<ChangeBlock> observer) {
        observers.add(observer);}

    public synchronized void send(ChangeBlock block) {
        for(Consumer<ChangeBlock> observer : observers) {observer.accept(block);}}


}
