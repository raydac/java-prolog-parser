This parser initially was implemented as a part of [the JProl engine (it is an embeddable Prolog Java engine)](https://github.com/raydac/jprol) but then it was separated as an OSS project to allow for everyone to develop own Prolog engines in Java. The parser allows to parse Prolog sources written in the Edinburgh style (pay your attention, it supports only '%' comments). It can use below Java objects as char data sources: 

- java.lang.String
- java.io.InputStream?
- java.nio.channels.ReadableByteChannel?
- java.io.Reader 

# License
The Project is under [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)

# The 'Hello World' example
```Java
final PrologParser parser = new PrologParser(null);
  try {
         final PrologStructure structure = (PrologStructure) parser.nextSentence("hello :- world.");
         System.out.println(structure.getElement(0).getText()+' '+structure.getElement(1).getText());
      }catch(Exception unexpected){
         throw new RuntimeException(unexpected);
      }
```

# How to use with Maven
Since 1.3.1 version the Prolog Parser has been published through Maven Central and you can add it as a dependency into your pom.xml 
```
<dependency>
   <groupId>com.igormaznitsa</groupId>
   <artifactId>prologparser</artifactId>
   <version>1.3.2</version>
</dependency>
```
[It also can be downloaded as a prebuilt jar from maven central](http://search.maven.org/#search|gav|1|g%3A%22com.igormaznitsa%22%20AND%20a%3A%22prologparser%22)
# Donation
If you like the software you can make some donation to the author   
[![https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=Y7ND3NMFWGVWA)
