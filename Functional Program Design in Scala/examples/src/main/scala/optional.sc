trait Optional[+T]:
  def flatMap[U](f: T => Optional[U]): Optional[U]
//  def map[U](f: T => U): Optional[U] = flatMap(x => Optional(f(x)))
//  def map[U](f: T => U): Optional[U] = flatMap(x => f.andThen(Optional.apply)(x))
  def map[U](f: T => U): Optional[U] = flatMap(f andThen Optional.apply)

object Optional:
  def apply[T](value: T): Optional[T] = Some(value)

case class Some[+T](x: T) extends Optional[T]:
  override def flatMap[U](f: T => Optional[U]): Optional[U] = f(x)

case object None extends Optional[Nothing]:
  override def flatMap[U](f: Nothing => Optional[U]): Optional[U] = None

