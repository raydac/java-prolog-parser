package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;

/**
 * Listener gets notifications from parser for met parsed comments if detection flag is on
 *
 * @see ParserContext#FLAG_COMMENTS_AS_ATOMS
 * @since 2.2.0
 */
@FunctionalInterface
public interface TokenizedCommentListener {
  /**
   * Notification from parser that either a line comment or a block comment has got as a tokenizer result.
   *
   * @param parser  source prolog parser, must not be null
   * @param comment detected comment token as an atom, must not be null
   */
  void onCommentToken(PrologParser parser, TokenizerResult comment);
}
