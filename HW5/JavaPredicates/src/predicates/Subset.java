package predicates;

import java.util.Iterator;
import java.util.List;

public class Subset<T extends List<T>> implements Predicate<T> {
	
	private T referenceList;
	
	public Subset(T list) {
		referenceList=list;
	}

	@Override
	// Which should be a subset of the other
	public boolean accepts(T t) {
		Iterator i = t.listIterator();
		while(i.hasNext()) {
			if(!referenceList.contains(i.next())) {
				return false;
			}
		}
		return true;
	}

}