import java.io.File
import java.io.InputStream
import java.io.FileInputStream
package object common {
  type ??? = Nothing
  type *** = Any
  def subFile(file: File, children: String*) = 
  	children.foldLeft(file)((file, child)=> new File(file,child))
  	
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[InputStream] = {
    val classesDir = new File(getClass().getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile
    //println(projectDir)
    val resourceFile = subFile(projectDir, ("src" :: "test" :: "anagram" :: resourcePath):_*)
    if(resourceFile.exists)
      Some(new FileInputStream(resourceFile))
    else 
      None
  }
  

}