package be.hittepit.ffse.util

import org.scalatest.matchers.BePropertyMatchResult
import org.scalatest.matchers.BePropertyMatcher

trait TestUtil {

	/**
	 * Voir https://groups.google.com/forum/?fromgroups=#!topic/scalatest-users/UrdRM6XHB4Y
	 * Voir aussi beMatcher
	 */
	def anInstanceOf[T](implicit manifest: Manifest[T]) = {
	     val clazz = manifest.erasure.asInstanceOf[Class[T]]
	     new BePropertyMatcher[AnyRef] { 
	       def apply(left: AnyRef) = BePropertyMatchResult(clazz.isAssignableFrom(left.getClass), "an instance of " + clazz.getName) 
	     }
     }

}
