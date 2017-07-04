package apdl.generation

object IdGenerator {
  private var itr: Long = 0

  def nextVariable(id: String): String = {
    itr = itr + 1
    s"${id}_$itr"
  }
}
