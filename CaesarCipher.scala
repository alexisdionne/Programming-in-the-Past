object CaesarCipher {
  private val upper = 'A' to 'Z'
  private val lower = 'a' to 'z'

  /*def encode(text:String, key:Int) = text.map({
      case c if upper.contains(c) => rotate(upper, c, key)
      case c if lower.contains(c) => rotate(lower, c, key)
      case c => c
    })*/

  def encode(text:String, key:Int) : String = for (c <- text) yield (rotate(c, key))

  def decode(text:String, key:Int) = encode(text, key * -1)

  private def rotate(c:Char, key:Int): Char = {
    if ((c <= 90 && c >= 65)) { // upper case
      if (c + key < 65) (c + key +26).toChar
      else {
        if ((c.toInt + key) % 91 <= key) (65 + ((c.toInt + key) % 91)).toChar
        else ((c.toInt + key) % 91).toChar
      }
    }
    else return c
  }

  def solve(text:String, maxShift:Int){
    for(i <- maxShift to 0 by -1){
      println("Caesar " + i + ": " + encode(text, i))
    }
  }

  def main(args: Array[String]) {
    var text = "The quick brown fox jumps over the lazy dog"
    text = text.toUpperCase()
    val encoded = CaesarCipher.encode(text, 3)
    println(encoded)
    println(CaesarCipher.decode(encoded, 3))
    println()
    solve(text, 26)
  }
}