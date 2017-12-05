package predicates;

public class GreaterThan<T extends Comparable<T>> implements Predicate<T>  {
	
	private T reference;
	
	public GreaterThan(T r) {
		reference = r;
	}
	
	public boolean accepts(T t) {
		return reference.compareTo(t)<=0;
	}

}
