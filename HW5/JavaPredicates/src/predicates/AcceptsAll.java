package predicates;

import java.util.List;

public class AcceptsAll<T> implements Predicate<T> {
	
	List<T> refrenceList;
	
	public AcceptsAll(List<T> l) {
		refrenceList =l;
	}

	@Override
	public boolean accepts(T t) {
		
		
	}

}
