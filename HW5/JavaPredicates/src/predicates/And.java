package predicates;

public class And<T> implements Predicate<T> {
	
	Predicate<T> [] refrencePredicates;
	
	public And(Predicate<T> ... list) {
		refrencePredicates = list;
	}

	@Override
	public boolean accepts(T t) {
		for(int i=0; i<refrencePredicates.length; i++) {
			if(!refrencePredicates[i].accepts(t)) {
				return false;
			}
		}
		return true;
	}
}