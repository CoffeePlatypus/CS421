package predicates;

public class Negation<T extends Predicate<T>> implements Predicate<T> {
	
	private T reference;
	
	public Negation(T r) {
		reference = r;
	}

	@Override
	public boolean accepts(T t) {
		return ! reference.accepts(t);
	}

}
