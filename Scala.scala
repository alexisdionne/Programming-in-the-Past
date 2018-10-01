object CaesarCipher {
  private val upper = 'A' to 'Z'
  private val lower = 'a' to 'z'

  def encode(text:String, key:Int) = text.map {
    case c if upper.contains(c) => rotate(upper, c, key)
    case c if lower.contains(c) => rotate(lower, c, key)
    case c => c
  }

  def decode(text:String, key:Int) = encode(text,-key)

  private def rotate(a:IndexedSeq[Char], c:Char, key:Int) = a((c-a.head+key+a.size)%a.size)

  def solve(text:String, maxShift:Int){
    for(i <- maxShift to 0 by -1){
      println("Caesar " + i + ": " + decode(text, i))
    }
  }

  def main(args: Array[String]) {
    val text="The quick brown fox jumps over the lazy dog"
    println("Plaintext  => " + text)
    val encoded = CaesarCipher.encode(text, 3)
    println("Encrypted text => " + encoded)
    println("Decrypted  => " + CaesarCipher.decode(encoded, 3))
    println()
    solve(text, 26)
  }
}
