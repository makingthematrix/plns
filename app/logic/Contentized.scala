package logic

trait Contentized {
  /** all fields except id serialized - used mainly when you first create a new SpeechPartPair with id == noId 
   *  and then you want to check if such a pair is already in the dictionary*/
  val contents = contentize
  /** @todo actually it might be the one place in the code where reflection would be of use */
  protected def contentize: String
  
  def compareContents(that: Contentized) = this.contents.compare(that.contents)
  
  override def ## = this.contents.##
  override def hashCode = ##
}