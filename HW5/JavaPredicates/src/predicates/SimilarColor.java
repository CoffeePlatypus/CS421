package predicates;

import java.awt.Color;

public class SimilarColor implements Predicate<Color> {
	private Color color;
	
	public SimilarColor(Color c) {
		color=c;
	}

	public boolean accepts(Color t) {
		return (color.getBlue() - t.getBlue()) + (color.getGreen() - t.getGreen()) +  (color.getRed() - t.getRed()) <= 30;
	 }
}
