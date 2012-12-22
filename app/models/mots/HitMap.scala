package models.mots

final class HitMap private() {
  private val hits : Array[Byte] = Array.fill(26)(0)

  def contains(hitMap : HitMap) : Boolean = {
    var i = 0
    while(i < 26) {
      if(hitMap.hits(i) > hits(i)) {
        return false;
      }
      i += 1
    }
    return true;
  }

  def isEmpty : Boolean = {
    hits.forall(_ == 0)
  }

  def total : Int = hits.sum

  def max : Int = hits.max
}

case object HitMap {
  def forWord(word : String) : HitMap = {
    val hm = new HitMap()

    for(c <- word.toLowerCase; i = indexOf(c) if i >= 0) {
      hm.hits(i) = (hm.hits(i) + 1.toByte).asInstanceOf[Byte]
    }

    hm
  }

  private def indexOf(char : Char) : Int = char match {
    case 'á' | 'à' | 'â' | 'ä'     => ('a' - 'a').toInt
    case 'é' | 'è' | 'ê' | 'ë'     => ('e' - 'a').toInt 
    case 'í' | 'ì' | 'î' | 'ï'     => ('i' - 'a').toInt
    case 'ó' | 'ò' | 'ô' | 'ö'     => ('o' - 'a').toInt
    case 'ú' | 'ù' | 'û' | 'ü'     => ('u' - 'a').toInt
    case 'ý' | 'ỳ' | 'ŷ' | 'ÿ'     => ('y' - 'a').toInt
    case 'ç'                       => ('c' - 'a').toInt
    case c if c >= 'a' && c <= 'z' => (c - 'a').toInt
    case _ => -1
  }

  
}
