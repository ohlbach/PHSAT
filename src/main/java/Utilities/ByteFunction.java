package Utilities;


@FunctionalInterface
public interface ByteFunction<I> {
    public byte apply(I x);
}
