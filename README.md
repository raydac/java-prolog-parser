[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Maven central](https://img.shields.io/badge/maven%20central-2.2.3-green.svg)](http://search.maven.org/#artifactdetails|com.igormaznitsa|java-prolog-parser|2.2.3|jar)
[![Java 11.0+](https://img.shields.io/badge/java-11.0%2b-green.svg)](https://bell-sw.com/pages/downloads/#jdk-21-lts)

[![Arthur's acres sanctuary donation](arthur_sanctuary_banner.png)](https://www.arthursacresanimalsanctuary.org/donate)

# Java Prolog Parser

A hand-written, dependency-free Java parser for Prolog sources in **Edinburgh syntax**. It reads Prolog text from any
`java.io.Reader` and produces a lazy stream of typed term objects (atoms, numbers, variables, structures, and lists),
with full operator-precedence handling and source positions for every term.

The library was originally written as the front end of [JProl](https://github.com/raydac/jprol), a Prolog engine in
Java. It was then extracted into a standalone module and published to Maven Central. A deep refactoring in 2018 made it
faster and more robust.

## Contents

- [Features](#features)
- [Installation](#installation)
- [Quick start](#quick-start)
- [Core concepts](#core-concepts)
- [Reading terms](#reading-terms)
- [The term model](#the-term-model)
- [Operators](#operators)
- [Parser flags](#parser-flags)
- [Numbers](#numbers)
- [Quoted atoms and strings](#quoted-atoms-and-strings)
- [Comments](#comments)
- [Source positions and tokens](#source-positions-and-tokens)
- [Error handling](#error-handling)
- [Limits, threading, and resources](#limits-threading-and-resources)
- [Building from source](#building-from-source)
- [License](#license)

## Features

- Pure Java 11 library with **no runtime dependencies**; it ships as a JPMS module and is compatible with Android
  API 32.
- Streaming parser: terms are read one at a time, so large files never have to fit in memory as a whole.
- Full **operator-precedence parsing** with `xfx`, `xfy`, `yfx`, `fy`, `fx`, `xf`, and `yf` operators.
- Ready-made operator sets for **ISO**, **SWI-Prolog**, **GNU Prolog**, **SICStus**, SWI CLP(FD), and GNU FD, plus your
  own
  custom operators.
- Arbitrary-precision numbers (`BigInteger` / `BigDecimal`), including `0x`, `0o`, `0b`, `0'c` character codes, and
  `1_000_000` digit groups.
- Single-quoted, double-quoted, and back-tick quoted atoms with escape sequences.
- Line `%` and block `/* */` comments, optionally delivered to your code as tokens.
- Curly-bracket blocks `{a, b}`, zero-arity structures `f()`, variables as functors `Call(x)`, and `'.'(H, T)` lists,
  all
  switchable through flags.
- Line and column positions for every term, plus access to the raw tokens of the last read term.
- Descriptive `PrologParserException` errors that carry line and column.

## Installation

The latest release is published to Maven Central.

**Maven**

```xml
<dependency>
  <groupId>com.igormaznitsa</groupId>
  <artifactId>java-prolog-parser</artifactId>
  <version>2.2.3</version>
</dependency>
```

**Gradle (Kotlin DSL)**

```kotlin
implementation("com.igormaznitsa:java-prolog-parser:2.2.3")
```

**Gradle (Groovy DSL)**

```groovy
implementation 'com.igormaznitsa:java-prolog-parser:2.2.3'
```

**Java Platform Module System**

The JAR is a named module. Add it to your `module-info.java`:

```java
module my.application {
  requires igormaznitsa.prolog.parser;
}
```

## Quick start

```java
import com.igormaznitsa.prologparser.DefaultParserContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.tokenizer.Op;
import java.io.StringReader;

public final class QuickStart {
  public static void main(final String[] args) throws Exception {
    final String source = "hello(world). some({1,2,3}). power(X,Y,Z) :- Z is X ** Y.";
    final ParserContext context = DefaultParserContext.of(ParserContext.FLAG_CURLY_BRACKETS, Op.SWI);

    try (PrologParser parser = new GenericPrologParser(new StringReader(source), context)) {
      parser.forEach(System.out::println);
    }
  }
}
```

Output:

```text
hello(world)
some({1 , 2 , 3})
power(X, Y, Z) :- Z is X ** Y
```

## Core concepts

The API has three main parts:

| Part    | Class                                                                    | Role                                                                             |
|---------|--------------------------------------------------------------------------|----------------------------------------------------------------------------------|
| Parser  | `PrologParser`, `GenericPrologParser`                                    | Reads clauses from a `Reader`. It is `Iterable<PrologTerm>` and `AutoCloseable`. |
| Context | `ParserContext`, `DefaultParserContext`, `ParserContextChain`            | Supplies the known operators and the syntax flags.                               |
| Terms   | `PrologTerm` and its subclasses in `com.igormaznitsa.prologparser.terms` | The parsed result tree.                                                          |

`PrologParser` is abstract. `GenericPrologParser` is the ready-to-use implementation; subclass `PrologParser` only when
you need to change parser behavior, such as whether the reader is closed.

A parser knows **no operators** by default, not even `:-`. Always give it an operator set that matches the dialect you
parse, for example `Op.ISO`, `Op.SWI`, or `Op.GNU`.

## Reading terms

### Iterate over a source

Each call to `next()` returns one clause, without its terminating dot.

```java
try (PrologParser parser = new GenericPrologParser(reader, context)) {
  while (parser.hasNext()) {
    final PrologTerm clause = parser.next();
    System.out.println(clause);
  }
}
```

`PrologParser` is also `Iterable<PrologTerm>`, so the enhanced `for` loop works too:

```java
for (final PrologTerm clause : parser) {
  process(clause);
}
```

### Use the Stream API

`stream()` returns a sequential, lazy `Stream<PrologTerm>`:

```java
final String facts = "parent(tom, bob). parent(bob, ann). age(tom, 72).";

try (PrologParser parser = new GenericPrologParser(new StringReader(facts),
    DefaultParserContext.of(ParserContext.FLAG_NONE, Op.ISO))) {

  final List<String> children = parser.stream()
      .filter(term -> term.getType() == TermType.STRUCT && term.getText().equals("parent"))
      .map(term -> ((PrologStruct) term).getTermAt(1).getText())
      .collect(Collectors.toList());

  System.out.println(children); // [bob, ann]
}
```

### Parse a file

Always set the charset explicitly. Non-ASCII atoms and variables are supported.

```java
final Path file = Path.of("program.pl");

try (PrologParser parser = new GenericPrologParser(
    Files.newBufferedReader(file, StandardCharsets.UTF_8),
    DefaultParserContext.of(ParserContext.FLAG_BLOCK_COMMENTS, Op.SWI))) {
  parser.forEach(System.out::println);
}
```

### Keep the reader open

`close()` closes the underlying reader by default. Override `isCloseReader()` when the reader belongs to someone else:

```java
final PrologParser parser = new GenericPrologParser(sharedReader, context) {
  @Override
  protected boolean isCloseReader() {
    return false;
  }
};
```

## The term model

Every parsed element is a `PrologTerm`. Use `getType()` to branch on the kind of term, then cast to the concrete class.

| `TermType` | Class          | Example                            | Notes                                          |
|------------|----------------|------------------------------------|------------------------------------------------|
| `ATOM`     | `PrologAtom`   | `hello`, `'Hello World'`, `"text"` | `getQuotation()` tells how it was quoted.      |
| `ATOM`     | `PrologInt`    | `42`, `0xFF`, `0'a`                | `getIntValue()` returns `BigInteger`.          |
| `ATOM`     | `PrologFloat`  | `3.14`, `2.0E-3`                   | `getFloatValue()` returns `BigDecimal`.        |
| `VAR`      | `PrologVar`    | `X`, `_Tail`, `_`                  | `isAnonymous()` is true for `_`.               |
| `STRUCT`   | `PrologStruct` | `f(a, B)`, `X is Y + 1`            | Operator expressions are structures too.       |
| `LIST`     | `PrologList`   | `[a, b \| T]`                      | A chain of head/tail cells.                    |
| `OPERATOR` | `Op`           | `:-`, `is`, `+`                    | Appears as the functor of operator structures. |

Numbers report `TermType.ATOM`. Check `instanceof PrologNumeric` (or `PrologInt` / `PrologFloat`) to tell numbers from
plain atoms.

### Inspect the arguments of a structure

```java
final String source =
    "rule(X, _, 'Hello World', \"str\", `bt`, 42, 3.14, [a,b|T], 123456789012345678901234567890).";

try (PrologParser parser = new GenericPrologParser(new StringReader(source),
    DefaultParserContext.of(ParserContext.FLAG_NONE, Op.ISO))) {

  final PrologStruct rule = (PrologStruct) parser.next();
  System.out.println(rule.getFunctor().getText() + "/" + rule.getArity());

  for (final PrologTerm argument : rule) {
    System.out.printf("%-6s %-11s %-9s %s%n",
        argument.getType(), argument.getClass().getSimpleName(), argument.getQuotation(), argument);
  }
}
```

Output:

```text
rule/9
VAR    PrologVar   NONE      X
VAR    PrologVar   NONE      _
ATOM   PrologAtom  SINGLE    'Hello World'
ATOM   PrologAtom  DOUBLE    "str"
ATOM   PrologAtom  BACK_TICK `bt`
ATOM   PrologInt   NONE      42
ATOM   PrologFloat NONE      3.14
LIST   PrologList  NONE      [a, b|T]
ATOM   PrologInt   NONE      123456789012345678901234567890
```

### Walk a list

`PrologList` is `Iterable`. Iteration yields every element, and an open tail, such as the `T` in `[a, b | T]`, comes
last.

```java
final PrologList list = (PrologList) rule.getTermAt(7);   // [a, b|T]

System.out.println(list.getHead()); // a
System.out.println(list.getTail()); // [b|T]

list.forEach(System.out::println);  // a, b, T
```

An empty list `[]` is a `PrologList` whose `isEmpty()` returns `true`.

### Split a clause into head and body

A rule is a structure whose functor is the `:-` operator. `flatComma` flattens a conjunction `(A, B, C)` into a list.

```java
final PrologStruct clause = (PrologStruct) parser.next();
// grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

if (clause.getFunctor().getType() == TermType.OPERATOR && clause.getText().equals(":-")) {
  final PrologTerm head = clause.getTermAt(0);
  final List<PrologTerm> goals = clause.getTermAt(1).flatComma(new ArrayList<>());

  System.out.println(head);   // grandparent(X, Z)
  System.out.println(goals);  // [parent(X, Y), parent(Y, Z)]
}
```

### Directives, queries, and DCG rules

Prefix operators produce structures of arity 1, so directives and queries are easy to recognize:

```java
final String source = ":- dynamic counter/1. greeting --> [hello], name. ?- member(X, [1,2]).";

try (PrologParser parser = new GenericPrologParser(new StringReader(source),
    DefaultParserContext.of(ParserContext.FLAG_NONE, Op.SWI))) {
  parser.forEach(term ->
      System.out.println(term + "   functor=" + term.getText() + "/" + term.getArity()));
}
```

Output:

```text
:- dynamic counter / 1   functor=:-/1
greeting --> [hello] , name   functor=-->/2
?- member(X, [1, 2])   functor=?-/1
```

### See how precedence shaped the tree

The parser builds the tree from operator precedence and associativity. A small recursive printer shows the result:

```java
static void dump(final PrologTerm term, final String indent) {
  if (term.getType() == TermType.STRUCT) {
    final PrologStruct struct = (PrologStruct) term;
    System.out.println(indent + struct.getText() + "  (precedence " + struct.getPrecedence() + ")");
    struct.forEach(argument -> dump(argument, indent + "  "));
  } else {
    System.out.println(indent + term + "  (" + term.getType() + ")");
  }
}
```

For `X is 1 + 2 * 3 - 4.` with `Op.ISO`, it prints:

```text
is  (precedence 700)
  X  (VAR)
  -  (precedence 500)
    +  (precedence 500)
      1  (ATOM)
      *  (precedence 400)
        2  (ATOM)
        3  (ATOM)
    4  (ATOM)
```

`*` binds tighter than `+` and `-`, and the left-associative `yfx` operators `+` and `-` group from the left.

### Turn terms back into text

`toString()` renders a term as Prolog source. It quotes atoms and escapes characters where needed, and it adds brackets
only where precedence requires them:

```java
// Source:  x :- (a ; b -> c), \+ d, X = 'Hello\nWorld', Y = - (1).
System.out.println(parser.next());
// x :- (a ; b -> c) , \+ d , X = 'Hello\nWorld' , Y = - 1
```

## Operators

### Predefined operator sets

The constants are defined in `com.igormaznitsa.prologparser.tokenizer.Op`:

| Constant             | Contents                                                      |
|----------------------|---------------------------------------------------------------|
| `Op.ISO`             | ISO Prolog standard operators.                                |
| `Op.SWI`             | `Op.ISO` plus SWI-Prolog operators (`Op.SWI_SPECIFIC`).       |
| `Op.GNU`             | `Op.ISO` plus GNU Prolog operators (`Op.GNU_SPECIFIC`).       |
| `Op.SICTUS_SPECIFIC` | SICStus Prolog extras; combine with `Op.ISO`.                 |
| `Op.SWI_CPL`         | SWI-Prolog CLP(FD) operators such as `#=`, `in`, `ins`, `..`. |
| `Op.GNU_FD`          | GNU Prolog finite-domain operators.                           |

Single operators such as `Op.ISO_CLAUSES`, `Op.ISO_OR`, `Op.MODIFIERS`, or `Op.GNU_STAR_THEN` are also public, so you
can
assemble a minimal set yourself.

Sets can be combined:

```java
final ParserContext context = DefaultParserContext.of(ParserContext.FLAG_NONE, Op.SWI, Op.SWI_CPL);

// puzzle(X,Y) :- [X,Y] ins 0..9, X + Y #= 10, X #> Y.
// is parsed and printed as:
// puzzle(X, Y) :- [X, Y] ins 0 .. 9 , X + Y #= 10 , X #> Y
```

### Define custom operators

`Op.make(precedence, associativity, names...)` creates an operator. Precedence must be in `0..1200`, and names must not
start with an upper-case letter, `_`, or whitespace.

```java
final DefaultParserContext context = new DefaultParserContext(ParserContext.FLAG_NONE)
    .addOps(Op.ISO)
    .addOps(
        Op.make(700, OpAssoc.XFX, "likes", "hates"),
        Op.make(200, OpAssoc.XF, "is_happy"));

try (PrologParser parser = new GenericPrologParser(
    new StringReader("john likes mary. john is_happy."), context)) {
  parser.forEach(term ->
      System.out.println(term + "   functor=" + term.getText() + " arity=" + term.getArity()));
}
```

Output:

```text
john likes mary   functor=likes arity=2
john is_happy   functor=is_happy arity=1
```

`Op.toString()` prints the operator as a Prolog directive, which is handy for logging:

```java
System.out.println(Op.make(700, OpAssoc.XFX, "likes", "hates")); // op(700, xfx, ['likes','hates']).
```

### Apply `op/3` directives while parsing

Terms are read lazily, so operators added to the context after one clause are already in effect for the next clause.
This lets you honor `:- op(...)` directives found in the source:

```java
final DefaultParserContext context = new DefaultParserContext(ParserContext.FLAG_NONE).addOps(Op.SWI);
final String source = ":- op(700, xfx, '===>').\nrule(a ===> b).";

try (PrologParser parser = new GenericPrologParser(new StringReader(source), context)) {
  for (final PrologTerm term : parser) {
    if (isOpDirective(term)) {
      final PrologStruct op = (PrologStruct) ((PrologStruct) term).getTermAt(0);
      context.addOps(Op.make(
          ((PrologInt) op.getTermAt(0)).getIntValue().intValue(),
          OpAssoc.findForName(op.getTermAt(1).getText()).orElseThrow(),
          op.getTermAt(2).getText()));
    } else {
      System.out.println(term); // rule(a ===> b)
    }
  }
}

static boolean isOpDirective(final PrologTerm term) {
  return term.getType() == TermType.STRUCT
      && term.getText().equals(":-")
      && term.getArity() == 1
      && ((PrologStruct) term).getTermAt(0).getText().equals("op")
      && ((PrologStruct) term).getTermAt(0).getArity() == 3;
}
```

Quote symbolic operator names in the directive itself (`'===>'`), because the name is not yet an operator when the
directive is read.

### Combine contexts

`ParserContextChain` merges several contexts. Their flags are OR-ed together, and operators are looked up in order, so
the
first context that knows a name wins.

```java
final ParserContext dialect = DefaultParserContext.of(ParserContext.FLAG_BLOCK_COMMENTS, Op.ISO);
final ParserContext domain = DefaultParserContext.of(ParserContext.FLAG_NONE, Op.make(700, OpAssoc.XFX, "likes"));

final ParserContext context = ParserContextChain.of(dialect, domain);
// "/* c */ a likes b."  ->  a likes b
```

### Write your own context

`ParserContext` is a small interface. Implement it to load operators from a database, share them across parsers, or
resolve them per source. `hasOpStartsWith` must return `true` for every prefix of every operator name, because the
tokenizer uses it to decide how far a symbolic token extends.

```java
public final class ReadOnlyOperators implements ParserContext {
  private final DefaultParserContext delegate;

  public ReadOnlyOperators(final List<Op> operators) {
    this.delegate = new DefaultParserContext(FLAG_BLOCK_COMMENTS).addOps(operators.toArray(Op[]::new));
  }

  @Override
  public boolean hasOpStartsWith(final PrologParser source, final String namePrefix) {
    return this.delegate.hasOpStartsWith(source, namePrefix);
  }

  @Override
  public OpContainer findOpForName(final PrologParser source, final String name) {
    return this.delegate.findOpForName(source, name);
  }

  @Override
  public int getFlags() {
    return this.delegate.getFlags();
  }
}
```

## Parser flags

Flags are bit masks defined in `ParserContext`; combine them with `|`.

| Flag                                         | Enables                                       | Example                    |
|----------------------------------------------|-----------------------------------------------|----------------------------|
| `FLAG_NONE`                                  | Strict Edinburgh syntax, nothing extra.       |                            |
| `FLAG_BLOCK_COMMENTS`                        | Block comments.                               | `/* comment */`            |
| `FLAG_CURLY_BRACKETS`                        | Curly-bracket terms.                          | `{a, b}`                   |
| `FLAG_ZERO_STRUCT`                           | Structures with no arguments.                 | `f()`                      |
| `FLAG_VAR_AS_FUNCTOR`                        | A variable used as a functor.                 | `Call(a, b)`               |
| `FLAG_ZERO_QUOTATION_CHARCODE`               | Character code literals.                      | `0'a` is `97`              |
| `FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR` | A space after `0'`.                           | `0' ` is `32`              |
| `FLAG_DOT2_AS_LIST`                          | `'.'/2` read as a list cell (see note below). | `'.'(1, [])` is `[1]`      |
| `FLAG_COMMENTS_AS_ATOMS`                     | Comments delivered to listeners.              | See [Comments](#comments). |

Line comments `%` are always recognized.

`FLAG_DOT2_AS_LIST` converts `'.'(H, T)` when it is a whole clause, a structure argument, or a list element; for
example,
`f('.'(1, '.'(2, [])))` becomes `f([1, 2])`. It does not convert an operand of an operator, so `L = '.'(1, [])` stays a
structure.

```java
final int flags = ParserContext.FLAG_BLOCK_COMMENTS
    | ParserContext.FLAG_CURLY_BRACKETS
    | ParserContext.FLAG_ZERO_STRUCT
    | ParserContext.FLAG_VAR_AS_FUNCTOR
    | ParserContext.FLAG_ZERO_QUOTATION_CHARCODE;

final ParserContext context = DefaultParserContext.of(flags, Op.ISO);

// Call(a,b). f(). X = 0'a. {a,b}. /* block */ ok.
// is parsed as:
//   Call(a, b)    STRUCT
//   f()           STRUCT
//   X = 97        STRUCT
//   {a , b}       STRUCT  (isCurlyBlock() == true)
//   ok            ATOM
```

Without the matching flag, the same input fails with a `PrologParserException`. For example, `f().` reports
`Empty structure is not allowed`.

## Numbers

Integers become `PrologInt` (backed by `BigInteger`), and floats become `PrologFloat` (backed by `BigDecimal`). Their
size
is limited only by available memory.

```java
// n(0x1F, 0o17, 0b1010, 1_000_000, 2.0E-3, 0'A, -7).
// with FLAG_ZERO_QUOTATION_CHARCODE

final PrologStruct numbers = (PrologStruct) parser.next();
for (final PrologTerm number : numbers) {
  System.out.println(number.getClass().getSimpleName() + " " + number);
}
```

Output:

```text
PrologInt 31
PrologInt 15
PrologInt 10
PrologInt 1000000
PrologFloat 0.0020
PrologInt 65
PrologInt -7
```

Read the exact value with `getIntValue()`, `getFloatValue()`, or the generic `PrologNumeric.getNumber()`:

```java
final BigInteger big = ((PrologInt) term).getIntValue();
final BigDecimal real = ((PrologFloat) term).getFloatValue();
```

Rules for digit groups: an underscore may appear between digits (`1_000`, `0xF_F`, `1_2.3_4e1_0`), but not directly
before
`.` or the exponent marker (`1_E+2` is an error).

## Quoted atoms and strings

| Quotation     | Syntax          | `Quotation` value |
|---------------|-----------------|-------------------|
| none          | `hello`         | `NONE`            |
| single quotes | `'Hello World'` | `SINGLE`          |
| double quotes | `"text"`        | `DOUBLE`          |
| back ticks    | `` `text` ``    | `BACK_TICK`       |

All three quoted forms produce a `PrologAtom`. `getText()` returns the unescaped content, while `toString()` returns the
quoted, escaped form. Backslash escapes such as `\n`, `\t`, `\\`, `\'`, and `\uXXXX` are supported.

```java
final PrologTerm atom = parser.next(); // 'Hello\nWorld'.

atom.getText();      // "Hello" + newline + "World"
atom.getQuotation(); // Quotation.SINGLE
atom.toString();     // 'Hello\nWorld'
```

The ISO doubled-quote form `'don''t'` is not supported; write `'don\'t'` instead.

## Comments

By default, comments are skipped. With `FLAG_COMMENTS_AS_ATOMS`, each comment is passed to every registered
`TokenizedCommentListener` as an atom quoted with `Quotation.COMMENT_LINE` or `Quotation.COMMENT_BLOCK`. This is useful
for documentation tools, formatters, and IDE plugins.

```java
final String source = "% line comment\nfact(1). /* block\ncomment */ fact(2).";
final List<String> comments = new ArrayList<>();

final TokenizedCommentListener collector = (parser, token) ->
    comments.add(token.getResult().getQuotation() + " at " + token.getLine() + ":" + token.getPos()
        + " -> [" + token.getResult().getText() + "]");

try (PrologParser parser = new GenericPrologParser(
    new StringReader(source),
    DefaultParserContext.of(ParserContext.FLAG_BLOCK_COMMENTS | ParserContext.FLAG_COMMENTS_AS_ATOMS, Op.ISO),
    List.of(collector))) {
  parser.forEach(System.out::println);
}

comments.forEach(System.out::println);
```

Output:

```text
fact(1)
fact(2)
COMMENT_LINE at 1:1 -> [ line comment]
COMMENT_BLOCK at 2:10 -> [ block
comment ]
```

Comments never appear inside the parsed terms; they are only delivered to listeners.

## Source positions and tokens

Every parsed term carries its line and column, both starting at 1. Positions are `-1` when unknown.

```java
// Source:
// a.
//   likes(john,
//         mary).
parser.next();
final PrologStruct likes = (PrologStruct) parser.next();

likes.getLine();               // 2
likes.getPos();                // 3
likes.getTermAt(1).getLine();  // 3   ("mary")
likes.getTermAt(1).getPos();   // 9
```

`getLastTokens()` returns the raw tokens that made up the term returned by the last `next()` call. Each token includes
the original text with any leading whitespace, which helps with syntax highlighting or source-preserving
transformations:

```java
for (final TokenizerResult token : parser.getLastTokens()) {
  System.out.printf("%-8s %-6s %d:%d%n",
      token.getResult().getType(), token.getResult().getText(), token.getLine(), token.getPos());
}
```

Output:

```text
ATOM     likes  2:3
OPERATOR (      2:8
ATOM     john   2:9
OPERATOR ,      2:13
ATOM     mary   3:9
OPERATOR )      3:13
OPERATOR .      3:14
```

## Error handling

Syntax errors throw `PrologParserException`, an unchecked exception that carries the line and column of the problem.

```java
final String source = "ok(1).\nbroken(1, .\nok(2).";

try (PrologParser parser = new GenericPrologParser(new StringReader(source),
    DefaultParserContext.of(ParserContext.FLAG_NONE, Op.ISO))) {
  while (parser.hasNext()) {
    try {
      System.out.println(parser.next());
    } catch (PrologParserException ex) {
      if (ex.hasValidPosition()) {
        System.err.printf("Syntax error at %d:%d: %s%n", ex.getLine(), ex.getPos(), ex.getMessage());
      }
      break;
    }
  }
}
```

Output:

```text
ok(1)
Syntax error at 3:6: Non-ended clause
```

More examples of messages (`toString()` appends `[line:column]`):

| Input                             | Message                                                    |
|-----------------------------------|------------------------------------------------------------|
| `a :- b :- c.`                    | `Operator precedence clash or missing operator: [:-][1:8]` |
| `f().` without `FLAG_ZERO_STRUCT` | `Empty structure is not allowed[1:2]`                      |

Behavior worth knowing:

- `hasNext()` returns `false` only at the real end of input. If the remaining text is malformed, `hasNext()` returns
  `true` and the following `next()` throws, so errors are never silently dropped.
- Calling `next()` on an exhausted parser throws `java.util.NoSuchElementException`.
- The parser does not recover from syntax errors. The rest of the input may already be consumed, and later calls to
  `hasNext()` return `false`. Treat the first error as the end of parsing.

## Limits, threading, and resources

**Token size limit.** To protect against huge or hostile input, override `getMaxTokenizerBufferLength()` in your
context.
A token longer than the limit raises `CharBufferOverflowException`, which is a subclass of `PrologParserException`.

```java
final ParserContext limited = new DefaultParserContext(ParserContext.FLAG_NONE) {
  @Override
  public int getMaxTokenizerBufferLength() {
    return 4096;
  }
};
```

When contexts are chained, the smallest limit applies.

**Threading.** A parser instance is not thread-safe; use one parser per thread and source. `DefaultParserContext` is not
thread-safe either; do not add operators to it while another thread is parsing with it. Parsed terms are mutable objects
(positions, list cells, and structure elements can be changed), so do not share them across threads without
synchronization.

**Resources.** Always close the parser, preferably with try-with-resources. By default, closing the parser also closes
the
reader (see [Keep the reader open](#keep-the-reader-open)).

## Building from source

Requirements: JDK 11 or newer and Maven 3.8 or newer.

```bash
git clone https://github.com/raydac/java-prolog-parser.git
cd java-prolog-parser
mvn clean install
```

`mvn verify` also runs SpotBugs, and the test phase checks Android API compatibility with Animal Sniffer.

## License

The library is distributed under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0).
