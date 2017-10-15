package scheduler

case class SoftwarePlatform() {

}

object SoftwarePlatform {

	/* Input: Assignments 'jobs' with their durations and requirements
	 * Output: Estimated delivery time or a circular requirement
	 *
	 * jobs' is a List because the order matters. Circular requirement needs the jobs we've
	 * encountered, which can be accomplished by splicing our list at the index we are currently on
	 * when looping through the list.
	 */
	def estimatedDeliveryTime(jobs: List[Job]): Int = {
			???
	}
}
