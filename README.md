[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Maven central](https://maven-badges.herokuapp.com/maven-central/com.igormaznitsa/java-prolog-parser/badge.svg)](http://search.maven.org/#artifactdetails|com.igormaznitsa|java-prolog-parser|2.0.2|jar)
[![Java 8.0+](https://img.shields.io/badge/java-8.0%2b-green.svg)](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-red.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2)
[![Yandex.Money donation](https://img.shields.io/badge/donation-Я.деньги-yellow.svg)](http://yasobe.ru/na/iamoss)


# Preword

During my development of Java prolog engine (JProl) I had developed parser to parse sources written in Edinburgh style, I decided to extract the module into separated project and 1.0.0 version was published. In 2018 I had decided to make deep refactoring and 2.0.0 version was published. The Parser allows build custom prolog parsers with support of operators and some modern prolog features.

# Features

It supports
 - prolog operators
 - line and block commentaries
 - underline splitted numbers
 - curly blocks

# Maven dependency

It is a library without 3th side dependencies, it is published im the Maven central and can be added into project just with:
```
<dependency>
    <groupId>com.igormaznitsa</groupId>
    <artifactId>java-prolog-parser</artifactId>
    <version>2.0.2</version>
</dependency>
```

# How to use?
Parser implements stream which sequentially reads prolog terms from reader. By default PrologParser is abstract class but there is GenericPrologParser class provides minimalistic implementation.
```
Reader reader = new StringReader("hello(world). some({1,2,3}). power(X,Y,Z) :- Z is X ** Y.");
PrologParser parser = new GenericPrologParser(reader, new DefaultParserContext(ParserContext.FLAG_CURLY_BRACKETS, Op.SWI));
parser.forEach(System.out::println);
```

# Supported Prolog terms
The parser supports
 - atoms (class PrologAtom)
 - numbers (classes PrologInt and PrologFloat), they are based on big numbers so that their size is not restricted
 - lists (class PrologList)
 - structures (class PrologStruct)
 - variables (class PrologVar)

Supported quotations:
 - single quote `'`
 - special flag turns on whitespace support in single quote mode
 - double quote `"`
 - back tick
 
Bracketed empty-functor structures represented by structure with functor either `{}` or `()` (depends on bracket type).
