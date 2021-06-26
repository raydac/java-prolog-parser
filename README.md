[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Maven central](https://maven-badges.herokuapp.com/maven-central/com.igormaznitsa/java-prolog-parser/badge.svg)](http://search.maven.org/#artifactdetails|com.igormaznitsa|java-prolog-parser|2.0.2|jar)
[![Java 8.0+](https://img.shields.io/badge/java-8.0%2b-green.svg)](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-cyan.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2)
[![YooMoney donation](https://img.shields.io/badge/donation-Yoo.money-blue.svg)](https://yoomoney.ru/to/41001158080699)


# Pre-word

During [my experiments in Prolog with Java](https://github.com/raydac/jprol), I developed some parser to process Prolog sources written in Edinburgh style. I decided that it is useful stuff, and it would be good to be prepared as separated independent module published in Maven central. In 2018 deep refactoring made the parser better and stronger.

# Features

It supports
 - prolog operators
 - line and block commentaries
 - underline splitted numbers
 - curly blocks

# Maven dependency

The parser is a pure Java library without any 3-th side dependency, it is published im the Maven central and can be injected into project just by adding:
```
<dependency>
    <groupId>com.igormaznitsa</groupId>
    <artifactId>java-prolog-parser</artifactId>
    <version>2.0.2</version>
</dependency>
```

# How to use?
Parser implements stream which sequentially reads prolog terms provided by reader. By default, PrologParser is abstract class but there is pre-defined implementation GenericPrologParser for common cases.
```
    Reader reader = new StringReader("hello(world). some({1,2,3}). power(X,Y,Z) :- Z is X ** Y.");
    ParserContext context = DefaultParserContext.of(ParserContext.FLAG_CURLY_BRACKETS, Op.SWI);
    PrologParser parser = new GenericPrologParser(reader, context);
    parser.forEach(System.out::println);
```

# Supported Prolog terms
Supported tokens:
 - atoms (class PrologAtom)
 - numbers (classes PrologInt and PrologFloat), they are based on java big number classes so that their size is limited mainly only by heap
 - lists (class PrologList)
 - structures (class PrologStruct)
 - variables (class PrologVar)

Supported quotations:
 - single quote `'`
 - special flag turns on whitespace support in single quote mode
 - double quote `"`
 - back tick
 
Bracketed empty-functor structures represented by structure with functor either `{}` or `()` (depends on bracket type).
