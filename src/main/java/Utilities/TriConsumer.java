package Utilities;

import java.util.Objects;
/**
 * Created by ohlbach on 06.02.2020.
 */

@FunctionalInterface
public interface TriConsumer<T, U, V> {
    void accept(T var1, U var2, V var3);

    default TriConsumer<T, U, V> andThen(TriConsumer<? super T, ? super U, ? super V> var1) {
        Objects.requireNonNull(var1);
        return (var2, var3, var4) -> {
            this.accept(var2, var3, var4);
            var1.accept(var2, var3, var4);
            };
        }
    }

