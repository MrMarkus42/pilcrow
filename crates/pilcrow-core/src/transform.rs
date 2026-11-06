//! Text transformations of the selection (case, whitespace, redaction) and text analysis
//! (brackets, character ranges, summary). Every change is a single undo step.

use std::ops::{Range, RangeInclusive};

use ropey::Rope;

use crate::Document;

/// Replaces `range` with `f(text)` as one undo step and returns the new range.
fn transform(
    doc: &mut Document,
    range: Range<usize>,
    f: impl FnOnce(&str) -> String,
) -> Range<usize> {
    let text = doc.slice(range.clone());
    let new = f(&text);
    if new == text {
        return range;
    }
    doc.seal_undo_group();
    let end = doc.replace(range.clone(), &new);
    doc.seal_undo_group();
    range.start..end
}

/// The selection, or the whole document if it is empty.
fn selection_or_all(doc: &Document, selection: &Range<usize>) -> Range<usize> {
    if selection.is_empty() {
        0..doc.text().len_chars()
    } else {
        selection.clone()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Case {
    Upper,
    Lower,
    /// First letter of every word upper case, the rest lower case.
    Proper,
    /// Like `Proper`, but keeps existing upper case letters.
    ProperBlend,
    Sentence,
    SentenceBlend,
    Invert,
    Random(u64),
}

pub fn change_case(doc: &mut Document, selection: &Range<usize>, case: Case) -> Range<usize> {
    if selection.is_empty() {
        return selection.clone();
    }
    transform(doc, selection.clone(), |text| convert_case(text, case))
}

fn convert_case(text: &str, case: Case) -> String {
    let upper = |c: char, out: &mut String| out.extend(c.to_uppercase());
    let lower = |c: char, out: &mut String| out.extend(c.to_lowercase());
    let mut out = String::with_capacity(text.len());
    match case {
        Case::Upper => return text.to_uppercase(),
        Case::Lower => return text.to_lowercase(),
        Case::Proper | Case::ProperBlend => {
            let mut in_word = false;
            for c in text.chars() {
                if c.is_alphanumeric() {
                    if in_word {
                        if case == Case::Proper {
                            lower(c, &mut out);
                        } else {
                            out.push(c);
                        }
                    } else {
                        upper(c, &mut out);
                    }
                    in_word = true;
                } else {
                    out.push(c);
                    in_word = c == '\'';
                }
            }
        }
        Case::Sentence | Case::SentenceBlend => {
            let mut sentence_start = true;
            for c in text.chars() {
                if c.is_alphabetic() {
                    if sentence_start {
                        upper(c, &mut out);
                    } else if case == Case::Sentence {
                        lower(c, &mut out);
                    } else {
                        out.push(c);
                    }
                    sentence_start = false;
                } else {
                    out.push(c);
                    if matches!(c, '.' | '!' | '?' | '\n') {
                        sentence_start = true;
                    }
                }
            }
        }
        Case::Invert => {
            for c in text.chars() {
                if c.is_uppercase() {
                    lower(c, &mut out);
                } else {
                    upper(c, &mut out);
                }
            }
        }
        Case::Random(seed) => {
            let mut state = seed | 1;
            for c in text.chars() {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                if state & 1 == 0 {
                    upper(c, &mut out);
                } else {
                    lower(c, &mut out);
                }
            }
        }
    }
    out
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Blank {
    TrimLeading,
    TrimBoth,
    /// Joins lines with a space.
    EolToSpace,
    TrimAndEolToSpace,
    TabsToSpaces,
    /// Turns runs of spaces ending at a tab stop into tabs; `true` = leading whitespace only.
    SpacesToTabs(bool),
}

/// Applies a whitespace operation to the selection, or to the whole document if it is empty.
pub fn blank_operation(
    doc: &mut Document,
    selection: &Range<usize>,
    operation: Blank,
    tab_width: usize,
) -> Range<usize> {
    let range = selection_or_all(doc, selection);
    let tab_width = tab_width.max(1);
    transform(doc, range, |text| {
        let lines = text.split('\n');
        match operation {
            Blank::TrimLeading => lines.map(str::trim_start).collect::<Vec<_>>().join("\n"),
            Blank::TrimBoth => lines.map(str::trim).collect::<Vec<_>>().join("\n"),
            Blank::EolToSpace => text.replace('\n', " "),
            Blank::TrimAndEolToSpace => lines
                .map(str::trim)
                .filter(|line| !line.is_empty())
                .collect::<Vec<_>>()
                .join(" "),
            Blank::TabsToSpaces => lines
                .map(|line| tabs_to_spaces(line, tab_width))
                .collect::<Vec<_>>()
                .join("\n"),
            Blank::SpacesToTabs(leading) => lines
                .map(|line| spaces_to_tabs(line, tab_width, leading))
                .collect::<Vec<_>>()
                .join("\n"),
        }
    })
}

fn tabs_to_spaces(line: &str, tab_width: usize) -> String {
    let mut out = String::with_capacity(line.len());
    let mut column = 0;
    for c in line.chars() {
        if c == '\t' {
            let spaces = tab_width - column % tab_width;
            out.extend(std::iter::repeat_n(' ', spaces));
            column += spaces;
        } else {
            out.push(c);
            column += 1;
        }
    }
    out
}

fn spaces_to_tabs(line: &str, tab_width: usize, leading_only: bool) -> String {
    let mut out = String::with_capacity(line.len());
    let (mut column, mut pending) = (0, 0);
    let mut in_leading = true;
    for c in line.chars() {
        let convert = !leading_only || in_leading;
        match c {
            ' ' if convert => {
                pending += 1;
                column += 1;
                if column % tab_width == 0 {
                    if pending > 1 {
                        out.push('\t');
                    } else {
                        out.push(' ');
                    }
                    pending = 0;
                }
            }
            '\t' => {
                pending = 0;
                out.push('\t');
                column += tab_width - column % tab_width;
            }
            _ => {
                out.extend(std::iter::repeat_n(' ', pending));
                pending = 0;
                out.push(c);
                column += 1;
                in_leading = false;
            }
        }
    }
    out.extend(std::iter::repeat_n(' ', pending));
    out
}

/// Replaces every visible character of the selection with `mask`, e.g. '█' or '●'.
pub fn redact(doc: &mut Document, selection: &Range<usize>, mask: char) -> Range<usize> {
    if selection.is_empty() {
        return selection.clone();
    }
    transform(doc, selection.clone(), |text| {
        text.chars()
            .map(|c| if c.is_whitespace() { c } else { mask })
            .collect()
    })
}

/// Brackets the scan for a matching bracket gives up after, to stay responsive.
const MAX_BRACKET_SCAN: usize = 1_000_000;

fn bracket_pair(c: char) -> Option<(char, char, bool)> {
    match c {
        '(' => Some(('(', ')', true)),
        '[' => Some(('[', ']', true)),
        '{' => Some(('{', '}', true)),
        ')' => Some(('(', ')', false)),
        ']' => Some(('[', ']', false)),
        '}' => Some(('{', '}', false)),
        _ => None,
    }
}

/// The bracket at or just before `pos` and its matching partner, as char positions.
pub fn matching_bracket(rope: &Rope, pos: usize) -> Option<(usize, usize)> {
    let len = rope.len_chars();
    let at = [pos, pos.wrapping_sub(1)]
        .into_iter()
        .find(|&p| p < len && bracket_pair(rope.char(p)).is_some())?;
    let (open, close, opening) = bracket_pair(rope.char(at))?;
    let mut depth = 0usize;
    if opening {
        for (offset, c) in rope.chars_at(at).take(MAX_BRACKET_SCAN).enumerate() {
            if c == open {
                depth += 1;
            } else if c == close {
                depth -= 1;
                if depth == 0 {
                    return Some((at, at + offset));
                }
            }
        }
    } else {
        let mut chars = rope.chars_at(at + 1);
        for offset in 0..MAX_BRACKET_SCAN.min(at + 1) {
            let c = chars.prev()?;
            if c == close {
                depth += 1;
            } else if c == open {
                depth -= 1;
                if depth == 0 {
                    return Some((at, at - offset));
                }
            }
        }
    }
    None
}

/// The text between the innermost brackets enclosing `pos` (without the brackets).
pub fn enclosing_brackets(rope: &Rope, pos: usize) -> Option<Range<usize>> {
    let mut stack = Vec::new();
    let mut chars = rope.chars_at(pos.min(rope.len_chars()));
    for offset in 1..=MAX_BRACKET_SCAN.min(pos) {
        let c = chars.prev()?;
        match bracket_pair(c) {
            Some((_, _, false)) => stack.push(c),
            Some((_, close, true)) => {
                if stack.last() == Some(&close) {
                    stack.pop();
                } else {
                    let open = pos - offset;
                    let (_, end) = matching_bracket(rope, open)?;
                    return Some(open + 1..end);
                }
            }
            None => {}
        }
    }
    None
}

/// Position of the next (or previous) char whose code point is in `range`, wrapping around.
pub fn find_char_in_range(
    rope: &Rope,
    from: usize,
    forward: bool,
    range: &RangeInclusive<u32>,
) -> Option<usize> {
    let len = rope.len_chars();
    let hit = |p: usize| range.contains(&(rope.char(p) as u32));
    if forward {
        (from..len).chain(0..from.min(len)).find(|&p| hit(p))
    } else {
        (0..from.min(len))
            .rev()
            .chain((from..len).rev())
            .find(|&p| hit(p))
    }
}

/// Longest markup tag that is recognized.
const MAX_TAG_LEN: usize = 4_096;

struct Tag {
    end: usize,
    name: Range<usize>,
    closing: bool,
    self_closing: bool,
}

/// Parses the markup tag starting with the `<` at `lt`.
fn parse_tag(rope: &Rope, lt: usize) -> Option<Tag> {
    let mut chars = rope.chars_at(lt);
    if chars.next()? != '<' {
        return None;
    }
    let mut pos = lt + 1;
    let mut c = chars.next()?;
    let closing = c == '/';
    if closing {
        pos += 1;
        c = chars.next()?;
    }
    let name_start = pos;
    if !c.is_alphabetic() {
        return None;
    }
    while c.is_alphanumeric() || matches!(c, ':' | '-' | '_' | '.') {
        pos += 1;
        c = chars.next()?;
    }
    let name = name_start..pos;
    let mut quote = None;
    let mut last = c;
    while pos - lt < MAX_TAG_LEN {
        match quote {
            Some(q) if c == q => quote = None,
            Some(_) => {}
            None => match c {
                '"' | '\'' => quote = Some(c),
                '>' => {
                    return Some(Tag {
                        end: pos + 1,
                        name,
                        closing,
                        self_closing: last == '/',
                    });
                }
                '<' => return None,
                _ => {}
            },
        }
        if !c.is_whitespace() {
            last = c;
        }
        pos += 1;
        c = chars.next()?;
    }
    None
}

/// The names of the markup tag around `pos` and of its partner (opening or closing tag).
pub fn matching_tags(rope: &Rope, pos: usize) -> Option<(Range<usize>, Range<usize>)> {
    let len = rope.len_chars();
    let mut p = (pos + 1).min(len);
    let mut chars = rope.chars_at(p);
    let tag = loop {
        if pos.saturating_sub(p) > MAX_TAG_LEN {
            return None;
        }
        let c = chars.prev()?;
        p -= 1;
        if c == '<' {
            break parse_tag(rope, p).filter(|tag| pos <= tag.end)?;
        }
        if c == '>' && p + 1 < pos {
            return None;
        }
    };
    if tag.self_closing {
        return None;
    }
    let same_name = |other: &Tag| {
        !other.self_closing && rope.slice(other.name.clone()) == rope.slice(tag.name.clone())
    };
    let mut depth = 1usize;
    if tag.closing {
        let mut chars = rope.chars_at(p);
        let mut at = p;
        while p - at < MAX_BRACKET_SCAN {
            let c = chars.prev()?;
            at -= 1;
            if c == '<'
                && let Some(other) = parse_tag(rope, at).filter(same_name)
            {
                if other.closing {
                    depth += 1;
                } else {
                    depth -= 1;
                    if depth == 0 {
                        return Some((tag.name, other.name));
                    }
                }
            }
        }
    } else {
        let chars = rope.chars_at(tag.end).take(MAX_BRACKET_SCAN);
        for (offset, c) in chars.enumerate() {
            let at = tag.end + offset;
            if c == '<'
                && let Some(other) = parse_tag(rope, at).filter(same_name)
            {
                if other.closing {
                    depth -= 1;
                    if depth == 0 {
                        return Some((tag.name, other.name));
                    }
                } else {
                    depth += 1;
                }
            }
        }
    }
    None
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Summary {
    /// Characters without line breaks.
    pub chars: usize,
    pub words: usize,
    pub lines: usize,
    /// Size on disk in the document's encoding and line ending.
    pub bytes: usize,
}

pub fn summary(doc: &Document) -> Summary {
    let rope = doc.text();
    let mut words = 0;
    let mut in_word = false;
    let mut newlines = 0;
    for c in rope.chars() {
        let word_char = c.is_alphanumeric() || c == '_';
        if word_char && !in_word {
            words += 1;
        }
        in_word = word_char;
        newlines += usize::from(c == '\n');
    }
    let bytes = doc
        .to_bytes()
        .map_or_else(|_| rope.len_bytes() + newlines, |bytes| bytes.len());
    Summary {
        chars: rope.len_chars() - newlines,
        words,
        lines: rope.len_lines(),
        bytes,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn doc(text: &str) -> Document {
        let mut doc = Document::new();
        doc.replace(0..0, text);
        doc
    }

    fn cased(text: &str, case: Case) -> String {
        convert_case(text, case)
    }

    #[test]
    fn matches_markup_tags() {
        let rope = Rope::from_str("<div class=\"a>b\"><p>x</p><br/><div></div></div>");
        let names = |pos| {
            matching_tags(&rope, pos).map(|(a, b)| {
                (
                    rope.slice(a.clone()).to_string(),
                    a.start,
                    rope.slice(b.clone()).to_string(),
                    b.start,
                )
            })
        };
        assert_eq!(names(1), Some(("div".into(), 1, "div".into(), 43)));
        assert_eq!(names(4), Some(("div".into(), 1, "div".into(), 43)));
        assert_eq!(names(23), Some(("p".into(), 23, "p".into(), 18)));
        assert_eq!(names(44), Some(("div".into(), 43, "div".into(), 1)));
        assert_eq!(names(28), None, "self-closing");
        assert_eq!(matching_tags(&Rope::from_str("a < b > c"), 3), None);
    }

    #[test]
    fn converts_case() {
        assert_eq!(cased("hELLO wORLD it's", Case::Proper), "Hello World It's");
        assert_eq!(cased("hELLO wORLD", Case::ProperBlend), "HELLO WORLD");
        assert_eq!(
            cased("hi THERE. how ARE you?", Case::Sentence),
            "Hi there. How are you?"
        );
        assert_eq!(cased("hi THERE. how", Case::SentenceBlend), "Hi THERE. How");
        assert_eq!(cased("aBc\u{df}", Case::Invert), "AbCSS");
        let random = cased("abcdefgh", Case::Random(7));
        assert_eq!(random.to_lowercase(), "abcdefgh");
        assert_ne!(random, "abcdefgh");
    }

    #[test]
    fn change_case_is_one_undo_step() {
        let mut d = doc("gr\u{fc}\u{df}e");
        assert_eq!(change_case(&mut d, &(0..5), Case::Upper), 0..6);
        assert_eq!(d.text().to_string(), "GR\u{dc}SSE");
        d.undo();
        assert_eq!(d.text().to_string(), "gr\u{fc}\u{df}e");
    }

    #[test]
    fn runs_blank_operations() {
        let run = |text: &str, op: Blank| {
            let mut d = doc(text);
            blank_operation(&mut d, &(0..0), op, 4);
            d.text().to_string()
        };
        assert_eq!(run("  a \n\tb ", Blank::TrimLeading), "a \nb ");
        assert_eq!(run("  a \n\tb ", Blank::TrimBoth), "a\nb");
        assert_eq!(run("a \nb", Blank::EolToSpace), "a  b");
        assert_eq!(run(" a \n\n b ", Blank::TrimAndEolToSpace), "a b");
        assert_eq!(run("a\tb\n\tc", Blank::TabsToSpaces), "a   b\n    c");
        assert_eq!(
            run("        x   y  z", Blank::SpacesToTabs(false)),
            "\t\tx\ty  z"
        );
        assert_eq!(
            run("        x       y", Blank::SpacesToTabs(true)),
            "\t\tx       y"
        );
        assert_eq!(run("a b", Blank::SpacesToTabs(false)), "a b");
    }

    #[test]
    fn redacts_visible_chars() {
        let mut d = doc("pin: 1234\nok");
        redact(&mut d, &(5..12), '\u{2588}');
        assert_eq!(
            d.text().to_string(),
            "pin: \u{2588}\u{2588}\u{2588}\u{2588}\n\u{2588}\u{2588}"
        );
    }

    #[test]
    fn matches_brackets() {
        let rope = Rope::from_str("f(a[1], {b}) )");
        assert_eq!(matching_bracket(&rope, 1), Some((1, 11)));
        assert_eq!(matching_bracket(&rope, 12), Some((11, 1)));
        assert_eq!(matching_bracket(&rope, 4), Some((3, 5)));
        assert_eq!(matching_bracket(&rope, 13), None);
        assert_eq!(matching_bracket(&rope, 0), None);
        assert_eq!(enclosing_brackets(&rope, 9), Some(9..10));
        assert_eq!(enclosing_brackets(&rope, 7), Some(2..11));
        assert_eq!(enclosing_brackets(&rope, 0), None);
    }

    #[test]
    fn finds_chars_in_range() {
        let rope = Rope::from_str("ab\u{e4}c\u{f6}");
        let non_ascii = 128..=255;
        assert_eq!(find_char_in_range(&rope, 0, true, &non_ascii), Some(2));
        assert_eq!(find_char_in_range(&rope, 3, true, &non_ascii), Some(4));
        assert_eq!(find_char_in_range(&rope, 5, true, &non_ascii), Some(2));
        assert_eq!(find_char_in_range(&rope, 4, false, &non_ascii), Some(2));
        assert_eq!(find_char_in_range(&rope, 0, true, &(0..=1)), None);
    }

    #[test]
    fn summarizes() {
        let mut document = doc("Hello, world!\nfoo_bar 42");
        document.set_line_ending(crate::LineEnding::Lf);
        let summary = summary(&document);
        assert_eq!(
            summary,
            Summary {
                chars: 23,
                words: 4,
                lines: 2,
                bytes: 24,
            }
        );
    }
}
