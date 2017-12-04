package predicates;

public class StartsWith implements Predicate<String> {
	
	private String reference;
	
	public StartsWith(String r) {
		reference = r;
	}
	
	public boolean accepts(String t) {
		return t.startsWith(reference);
	}

}
