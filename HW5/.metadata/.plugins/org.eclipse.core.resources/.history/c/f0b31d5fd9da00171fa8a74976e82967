package predicates;

import java.util.Iterator;
import java.util.List;
//doesnt implemtnt predicate of generic t
public class AcceptsAll<T> implements Predicate<Predicate<T>> {
	
	List<T> refrenceList;
	
	public AcceptsAll(List<T> l) {
		refrenceList =l;
	}
	// So the reference list is not of predicates but accepts takes a predicate as input?
	//
	@Override
	public boolean accepts(Predicate<T> t) {
		Iterator<T> it = refrenceList.iterator();
		while(it.hasNext()) {
			if(!t.accepts(it.next())) {
				return false;
			}
		}
		return true;	
	}
}
