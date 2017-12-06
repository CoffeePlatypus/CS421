package predicates;

public class Negation<T> implements Predicate<T> {
	
	private Predicate<T> reference;
	
	public Negation(Predicate<T> r) {
		reference = r;
	}

	@Override
	public boolean accepts(T t) {
		return ! reference.accepts(t);
	}

}
