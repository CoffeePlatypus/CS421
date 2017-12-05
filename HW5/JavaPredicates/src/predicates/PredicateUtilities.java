package predicates;

import java.util.List;

public class PredicateUtilities<T> {
	
	public List<T> listFilter(Predicate<T> pred,List<T> list) {
		int i = 0;
		while(i<list.size()) {
			if(pred.accepts(list.get(i))) {
				i++;
			}else{
				list.remove(i);
			}
		}
		return list;
	}
}
