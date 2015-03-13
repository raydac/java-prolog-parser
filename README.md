The Java Prolog Parser was developed as a part of the Prol engine (it is an embeddable Prolog Java engine) but lately it was separated as a free-standing open source project to allow for everyone to develop own Prolog engines in Java. The parser allows to parse prolog sources written in the Edinburgh style (pay your attention, it supports only '%' comments). It can use below Java objects as char data sources: 

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

# Usage from Maven
Since 1.3.1 version the Prolog Parser is accessible in the Maven Central and you can add it as a dependency into your pom.xml 
```
<dependency>
   <groupId>com.igormaznitsa</groupId>
   <artifactId>prologparser</artifactId>
   <version>1.3.2</version>
</dependency>
```
[It can be downloaded as a prebuilt jar from maven central](http://search.maven.org/#search|gav|1|g%3A%22com.igormaznitsa%22%20AND%20a%3A%22prologparser%22)
