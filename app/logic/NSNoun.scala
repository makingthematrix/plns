package logic

import Decl._
import IgnoredNumber._

object NSNoun extends NounGenerator("ns") {
  val HARD_MASCULINE_PERSON = new DeclensionPattern("ns","HARD_MASCULINE_PERSON","brat") { // "brat"
    override def suffices() = Map(
      NOMS -> "", GENS -> "a", DATS -> "u", ACCS -> "a", VOCS -> "e", LOCS -> "u", INSS -> "om",
      NOMP -> "i", GENP -> "ov", DATP -> "am", ACCP -> "y", VOCP -> "i", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }

  val MASCULINE_A = new DeclensionPattern("ns","MASCULINE_A","vladyka") { // "vladyka"
    override def suffices() = Map(
      NOMS -> "a", GENS -> "y", DATS -> "ie", ACCS -> "u", VOCS -> "o", LOCS -> "ie", INSS -> "oj",
      NOMP -> "i", GENP -> "ov", DATP -> "am", ACCP -> "y", VOCP -> "i", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }

  val HARD_MASCULINE_OBJECT = new DeclensionPattern("ns","HARD_MASCULINE_OBJECT","grad") { // "grad"
    override def suffices() = Map(
      NOMS -> "", GENS -> "a", DATS -> "u", ACCS -> "", VOCS -> "e", LOCS -> "u", INSS -> "om",
      NOMP -> "y", GENP -> "ov", DATP -> "am", ACCP -> "y", VOCP -> "y", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }

  val SOFT_MASCULINE_PERSON = new DeclensionPattern("ns","SOFT_MASCULINE_PERSON","muž") { // "muž"
    override def suffices() = Map(
      NOMS -> "", GENS -> "a", DATS -> "u", ACCS -> "a", VOCS -> "u", LOCS -> "u", INSS -> "em",
      NOMP -> "i", GENP -> "ev", DATP -> "am", ACCP -> "e", VOCP -> "i", LOCP -> "ah", INSP -> "ami" 
    )

    patternMap.put(this.id, this)
  }

  val SOFT_MASCULINE_OBJECT = new DeclensionPattern("ns","SOFT_MASCULINE_OBJECT","kraj") { // "kraj"
    override def suffices() = Map(
      NOMS -> "", GENS -> "a", DATS -> "u", ACCS -> "", VOCS -> "u", LOCS -> "u", INSS -> "em",
      NOMP -> "i", GENP -> "ev", DATP -> "am", ACCP -> "e", VOCP -> "i", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }

  val HARD_FEMININE = new DeclensionPattern("ns","HARD_FEMININE","žena") { // "žena"
    override def suffices() = Map(
      NOMS -> "a", GENS -> "y", DATS -> "ie", ACCS -> "u", VOCS -> "o", LOCS -> "ie", INSS -> "oj",
      NOMP -> "y", GENP -> "", DATP -> "am", ACCP -> "y", VOCP -> "y", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }

  val SOFT_FEMININE = new DeclensionPattern("ns","SOFT_FEMININE","dusa") { // "dusa"
    override def suffices() = Map(
      NOMS -> "a", GENS -> "e", DATS -> "i", ACCS -> "u", VOCS -> "e", LOCS -> "i", INSS -> "ej",
      NOMP -> "e", GENP -> "ej", DATP -> "am", ACCP -> "e", VOCP -> "e", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }

  val ACCORD_FEMININE = new DeclensionPattern("ns","ACCORD_FEMININE","kost") { // "kost"
    override def suffices() = Map(
      NOMS -> "", GENS -> "i", DATS -> "i", ACCS -> "", VOCS -> "i", LOCS -> "i", INSS -> "ju",
      NOMP -> "i", GENP -> "ij", DATP -> "im", ACCP -> "i", VOCP -> "i", LOCP -> "ih", INSP -> "imi"
    )

    patternMap.put(this.id, this)
  }

  val HARD_NEUTER = new DeclensionPattern("ns","HARD_NEUTER","selo") { // "selo"
    override def suffices() = Map(
      NOMS -> "o", GENS -> "a", DATS -> "u", ACCS -> "o", VOCS -> "o", LOCS -> "u", INSS -> "om",
      NOMP -> "a", GENP -> "", DATP -> "am", ACCP -> "a", VOCP -> "a", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }

  val SOFT_NEUTER = new DeclensionPattern("ns","SOFT_NEUTER","pole") { // "pole"
    override def suffices() = Map(
      NOMS -> "e", GENS -> "a", DATS -> "u", ACCS -> "e", VOCS -> "e", LOCS -> "u", INSS -> "em",
      NOMP -> "a", GENP -> "ej", DATP -> "am", ACCP -> "a", VOCP -> "a", LOCP -> "ah", INSP -> "ami"
    )

    patternMap.put(this.id, this)
  }
 
  def participle(word: String) = new Noun(word.substring(0, word.length()-1),SOFT_NEUTER,NONE,"ns")
}