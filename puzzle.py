"""
In this Mr.Moose puzzle, we must reconstruct a jarbled sentence. We know the number of words in the
solution sentence, we know the length of each word in the sentence, and we have a group of letters
that we can use to spell the solution sentence.
"""


import copy
import ipdb
import json
import numpy as np
import time
from collections import Counter
from pathlib import Path


# The number of words in the solution sentence.
NUM_WORDS = 6
# The length of each word in the solution sentence, presumably in order.
WORD_LENGTHS = sorted([6, 6, 4, 5, 7, 10], reverse=True)
# The characters we can use to spell the solution sentence with.
CHARS = Counter('mfctosesannhddcoieisanartheowgvtesebhr')

# The filename of the word-list. Contains lots of English words, hopefully includes all of the words
# needed to create the solution sentence.
WORDS_LIST_FNAME = 'common_words_list.txt'
# The filename of the words-by-length-dict. Contains a map from length of word to a list of words of
# that length. Constructed from the word-list.
WORDS_BY_LENGTH_DICT_FNAME = 'common_words_by_length_dict.json'
# The filename of the spellable-words-by-length-dict. Contains a map from length of word to a list
# of words of that length that can be spelled from the characters in `CHARS` and are not of a length
# that does not exist in `WORD_LENGTHS`. Constructed from the words-by-length-dict.
SPELLABLE_WORDS_BY_LENGTH_DICT_FNAME = 'common_spellable_words_by_length_dict.json'

SOLUTIONS_FNAME = 'solutions.txt'


LENGTH_ORDER_TO_SOLUTION_ORDER_1 = [2, 3, 5, 4, 1, 0]
LENGTH_ORDER_TO_SOLUTION_ORDER_2 = [3, 2, 5, 4, 1, 0]


class Word():
    def __init__(self, string):
        self.string = string
        self.chars = Counter(string)


class WordTreeRoot():
    def __init__(self):
        self.word = Word(None)
        self.children = []
        self.parent = None
        self.depth = 0

    def get_ancestry(self):
        return []


class WordNode():
    def __init__(self, word, parent):
        self.word = word
        self.children = []
        self.parent = parent
        self.depth = parent.depth + 1

    def get_ancestry(self):
        return self.parent.get_ancestry() + [self.word.string]



def get_words_list():
    print('Loading words-list from text file: {}'.format(WORDS_LIST_FNAME))
    words_list = open(WORDS_LIST_FNAME, 'r').read().splitlines()
    print('    Words-list has {} words.'.format(len(words_list)))
    return words_list


def get_words_by_length_dict(overwrite_current_json=False):
    # If the JSON file exists, and the user doesn't specifically specify to overwite the JSON file,
    # then read from it to get `words_by_length_dict`.
    if Path(WORDS_BY_LENGTH_DICT_FNAME).exists() and not overwrite_current_json:
        print('Loading words-by-length-dictionary from JSON file: {}'.format(
                WORDS_BY_LENGTH_DICT_FNAME))
        words_by_length_dict = json.load(open(WORDS_BY_LENGTH_DICT_FNAME, 'r'))
        # Convert loaded keys from strings to ints.
        words_by_length_dict = {int(key):val for key,val in words_by_length_dict.items()}
    else:
        # Parse the `words_list` to generate `words_by_length_dict`, then save it to a JSON file.
        words_list = get_words_list()
        words_by_length_dict = {}
        print('Parsing words-list to create words-by-length-dictionary.')
        total_num_words = len(words_list)
        for idx, word in enumerate(words_list):
            word_length = len(word)
            words_by_length_dict[word_length] = words_by_length_dict.get(word_length, []) + [word]
            if (idx % int(total_num_words / 10) == 0) or (idx == total_num_words - 1):
                print('    Parsed {}/{} words.'.format(idx, total_num_words))  # progress report

        print('Saving words-by-length-dictionary to JSON file: {}'.format(
                    WORDS_BY_LENGTH_DICT_FNAME))
        with open(WORDS_BY_LENGTH_DICT_FNAME, 'w') as fp:
            json.dump(words_by_length_dict, fp)

    print('    Words-by-length-dict has (len: # words):\n{}'.format(
            '\n'.join(['        {}: {}'.format(word_length, len(words)) for word_length, words in
            sorted(words_by_length_dict.items())])))

    return words_by_length_dict


def is_word_spellable_from_chars(word, chars):
    for char, count in word.chars.items():
        if count > chars.get(char, 0):
            return False
    return True


def is_string_word_spellable_from_chars(string_word, chars):
    word = Word(string_word)
    return is_word_spellable_from_chars(word, chars)


def get_spellable_words_by_length_dict(overwrite_current_json=False):
    # If the JSON file exists, and the user doesn't specifically specify to overwite the JSON file,
    # then read from it to get `words_by_length_dict`.
    if Path(SPELLABLE_WORDS_BY_LENGTH_DICT_FNAME).exists() and not overwrite_current_json:
        print('Loading spellable-words-by-length-dictionary from JSON file: {}'.format(
                SPELLABLE_WORDS_BY_LENGTH_DICT_FNAME))
        spellable_words_by_length_dict = json.load(open(SPELLABLE_WORDS_BY_LENGTH_DICT_FNAME, 'r'))
        # Convert loaded keys from strings to ints.
        spellable_words_by_length_dict = {int(key):val for key,val in
                spellable_words_by_length_dict.items()}
    else:
        # Parse `words_by_length_dict` to generate `spellable_words_by_length_dict`, then save it to
        # a JSON file.
        words_by_length_dict = get_words_by_length_dict(overwrite_current_json)
        spellable_words_by_length_dict = {}
        print('Parsing words-by-length-dict to create spellable-words-by-length-dict.')
        for word_length, words in words_by_length_dict.items():
            if word_length in WORD_LENGTHS:
                for word in words:
                    if is_string_word_spellable_from_chars(word, CHARS):
                        spellable_words_by_length_dict[word_length] = \
                                spellable_words_by_length_dict.get(word_length, []) + [word]

        print('Saving spellable-words-by-length-dictionary to JSON file: {}'.format(
                    SPELLABLE_WORDS_BY_LENGTH_DICT_FNAME))
        with open(SPELLABLE_WORDS_BY_LENGTH_DICT_FNAME, 'w') as fp:
            json.dump(spellable_words_by_length_dict, fp)

    print('    Spellable-words-by-length-dict has (len, # words):\n{}'.format(
            '\n'.join(['        {}: {}'.format(word_length, len(words)) for word_length, words in
            sorted(spellable_words_by_length_dict.items())])))

    return spellable_words_by_length_dict


def keep_top_percentage_of_frequent_words(words_by_length_dict, percent):
    wbld = {}
    for word_length, words in words_by_length_dict.items():
        num_words = len(words)
        for idx, word in enumerate(words):
            if idx < num_words * percent:
                wbld[word_length] = wbld.get(word_length, []) + [word]
    return wbld


def add_counters_to_words_by_length_dict(words_by_length_dict):
    words_by_length_dict_with_counters = {}
    for word_length, words in words_by_length_dict.items():
        for word in words:
            words_by_length_dict_with_counters[word_length] = \
                    words_by_length_dict_with_counters.get(word_length, []) + [Word(word)]
    return words_by_length_dict_with_counters


def get_chars_without_word_letters(word, chars):
    chars_copy = chars.copy()
    chars_copy.subtract(word.chars)
    return chars_copy


def get_word_lengths_without_length(length, word_lengths):
    word_lengths_copy = word_lengths.copy()
    word_lengths_copy.remove(length)
    return word_lengths_copy


def build_word_tree(solutions, parent_word_node, words_by_length_dict, chars, word_lengths):
    if len(word_lengths) == 0:
        solution = parent_word_node.get_ancestry()
        solutions.append(solution)
        num_solutions = len(solutions)
        print('Writing solution {} to file.'.format(num_solutions))
        solution_sentence = ' '.join([solution[idx] for idx in
                LENGTH_ORDER_TO_SOLUTION_ORDER_1])
        open(SOLUTIONS_FNAME, 'a').write(solution_sentence + '\n')
        return

    longest_word_length = max(word_lengths)
    for word in words_by_length_dict[longest_word_length]:
        if is_word_spellable_from_chars(word, chars):
            remaining_chars = get_chars_without_word_letters(word, chars)
            remaining_word_lengths = get_word_lengths_without_length(longest_word_length,
                    word_lengths)
            parent_word_node.children.append(build_word_tree(solutions,
                    WordNode(word, parent_word_node), words_by_length_dict, remaining_chars,
                    remaining_word_lengths))




def main():
    solutions = []
    open(SOLUTIONS_FNAME, 'w').write('')
    word_tree_root = WordTreeRoot()
    words_by_length_dict = get_spellable_words_by_length_dict()
    words_by_length_dict = keep_top_percentage_of_frequent_words(words_by_length_dict, .03)
    words_by_length_dict = add_counters_to_words_by_length_dict(words_by_length_dict)
    build_word_tree(solutions, word_tree_root, words_by_length_dict, CHARS, WORD_LENGTHS)


if __name__ == '__main__':
    with ipdb.launch_ipdb_on_exception():
        main()
