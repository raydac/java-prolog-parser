[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Maven central](https://maven-badges.herokuapp.com/maven-central/com.igormaznitsa/prologparser/badge.svg)](http://search.maven.org/#artifactdetails|com.igormaznitsa|prologparser|1.3.2|jar)
[![Java 6.0+](https://img.shields.io/badge/java-6.0%2b-green.svg)](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
[![Android 2.0+](https://img.shields.io/badge/android-2.0%2b-green.svg)](http://developer.android.com/sdk/index.html)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-red.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2)
[![Yandex.Money donation](https://img.shields.io/badge/donation-Я.деньги-yellow.svg)](http://yasobe.ru/na/iamoss)


This parser initially was developed as a component of [the JProl engine (it is an embeddable Prolog Java engine)](https://github.com/raydac/jprol) but then I decided that it was a good idea to form the parser as separated OSS project and allow to everione develop own Java based Prolog engine.   
The Parser parses Prolog sources written in Edinburgh style (pay your attention, it uses '%' for comments) into some AST (abstract syntax tree). It can use listed Java objects as data sources for chars: 
- java.lang.String
- java.io.InputStream
- java.nio.channels.ReadableByteChannel
- java.io.Reader

# License
The Project is published under [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)

# Parsing of 'Hello World'
The Parser will parse a simple prolog predicate into tree and it is possible to get elements of the tree. 
```java
    PrologParser parser = new PrologParser(null);
    PrologStructure rule = (PrologStructure) parser.nextSentence("hello :- world,!.");
    PrologStructure and = (PrologStructure) rule.getElement(1);
    System.out.println(rule.getElement(0).getText()+' '+and.getElement(0).getText()+and.getElement(1).getText());
```
It will print
```
hello world!
```

# How to use with Maven
To use with Maven just add the parser as dependency 
```
<dependency>
   <groupId>com.igormaznitsa</groupId>
   <artifactId>prologparser</artifactId>
   <version>1.3.2</version>
</dependency>
```
