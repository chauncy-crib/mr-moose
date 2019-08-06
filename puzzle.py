"""
In this Mr.Moose puzzle, we must reconstruct a jarbled sentence. We know the number of words in the
solution sentence, we know the length of each word in the sentence, and we have a group of letters
that we can use to spell the solution sentence.
"""


import copy
import ipdb
import json
import numpy as np
from pathlib import Path


# The number of words in the solution sentence.
NUM_WORDS = 6
# The length of each word in the solution sentence, presumably in order.
WORD_LENGTHS = sorted([6, 6, 4, 5, 7, 10], reverse=True)
# The characters we can use to spell the solution sentence with.
CHARS = sorted([char for char in 'mfctosesannhddcoieisanartheowgvte'])

# The filename of the word-list. Contains lots of English words, hopefully includes all of the words
# needed to create the solution sentence.
WORDS_LIST_FNAME = 'words_list.txt'
# The filename of the words-by-length-dict. Contains a map from length of word to a list of words of
# that length. Constructed from the word-list.
WORDS_BY_LENGTH_DICT_FNAME = 'words_by_length_dict.json'


def is_word_spellable_from_chars(word, remaining_chars):
    rem_chars = remaining_chars.copy()
    for letter in word:
        try:
            rem_chars.remove(letter)
        except(ValueError):
            return False
    return True


def remove_word_letters_from_chars(word, remaining_chars):
    rem_chars = copy.deepcopy(remaining_chars)
    for letter in word:
        rem_chars.remove(letter)
    return rem_chars


def get_words_list_from_words_file():
    return open(WORDS_LIST_FNAME, 'r').read().splitlines()


def get_words_by_length_dict(words_list, overwrite_current_json=False):
    # If the JSON file exists, and the user doesn't specifically specify to overwite the JSON file,
    # then read from it to get `words_by_length_dict`.
    if Path(WORDS_BY_LENGTH_DICT_FNAME).exists() and not overwrite_current_json:
        print('Loading words-by-length-dictionary from JSON file: {}'.format(
                WORDS_BY_LENGTH_DICT_FNAME))
        words_by_length_dict = json.load(open(WORDS_BY_LENGTH_DICT_FNAME, 'r'))
        # Convert loaded keys from strings to ints.
        return {int(key):val for key,val in words_by_length_dict.items()}

    # Parse the given `words_list` to generate `words_by_length_dict`, then save it to a JSON file.
    words_by_length_dict = {}
    print('Parsing words-list to create words-by-length-dictionary.')
    total_num_words = len(words_list)
    for idx, word in enumerate(words_list):
        word_len = len(word)
        words_by_length_dict[word_len] = words_by_length_dict.get(word_len, []) + [word]
        if (idx % int(total_num_words / 10) == 0) or (idx == total_num_words - 1):
            print('  Parsed {}/{} words.'.format(idx, total_num_words))  # progress report

    print('Saving words-by-length-dictionary to JSON file: {}'.format(
                WORDS_BY_LENGTH_DICT_FNAME))
    with open(WORDS_BY_LENGTH_DICT_FNAME, 'w') as fp:
        json.dump(words_by_length_dict, fp)

    return words_by_length_dict


def remove_unspellable_words(words_list, remaining_chars):
    spellable_words = []
    for word in words_list:
        if is_word_spellable_from_chars(word, remaining_chars) and len(word) in WORD_LENGTHS:
            spellable_words.append(word)
    return spellable_words


def init_possible_solutions(possible_solutions, words_by_length_dict):
    for word_idx in range(NUM_WORDS):
        if len(possible_solutions[word_idx]) == 0:  # skip pre-filled solutions (e.g. 'moose')
            cur_word_length = WORD_LENGTHS[word_idx]
            possible_solutions[word_idx] = words_by_length_dict[cur_word_length]
    return possible_solutions


def remove_word_from_possible_solutions(word, possible_solutions):
    pos_sols = copy.deepcopy(possible_solutions)
    word_length = len(word)
    for word_idx in range(NUM_WORDS):
        if WORD_LENGTHS[word_idx] == word_length:
            try:
                pos_sols[word_idx].remove(word)
            except(ValueError):
                pass
    return pos_sols


def is_word_possible(word, possible_solutions, remaining_chars):
    pos_sols = copy.deepcopy(possible_solutions)
    rem_chars = copy.deepcopy(remaining_chars)
    if not is_word_spellable_from_chars(word, rem_chars):
        return False
    rem_chars = remove_word_letters_from_chars(word, rem_chars)
    for word_idx in range(NUM_WORDS):
        for pos_word in pos_sols[word_idx]:
            if not is_word_possible(pos_word, pos_sols, rem_chars):
                pos_sols = remove_word_from_possible_solutions(word, pos_sols)
    if any([len(pos_words) == 0 for pos_words in pos_sols]):
        return False
    return True


def reduce_possible_solutions(possible_solutions, remaining_chars):
    pos_sols = copy.deepcopy(possible_solutions)
    rem_chars = copy.deepcopy(remaining_chars)
    for word_idx in range(NUM_WORDS):
        for pos_word in pos_sols[word_idx]:
            print('Seeing if {} is possible.'.format(pos_word))
            if not is_word_possible(pos_word, pos_sols, rem_chars):
                pos_sols = remove_word_from_possible_solutions(pos_word, pos_sols)
                print('Removed {}.'.format(pos_word))
            else:
                print('Kept {}.'.format(pos_word))
            print('Possible solution size: {}'.format([len(pos_words) for pos_words in pos_sols]))
        if len(pos_sols[word_idx]) == 1:
            rem_chars = remove_word_letters_from_chars(pos_sols[word_idx][0], rem_chars)
    return pos_sols, rem_chars


def main():
    remaining_chars = CHARS  # these are the characters we have to spell the solution sentence with
    possible_solutions = [[] for i in range(NUM_WORDS)]
    if is_word_spellable_from_chars('moose', remaining_chars):
        moose_idx = WORD_LENGTHS.index(len('moose'))
        possible_solutions[moose_idx] = ['moose']
        remaining_chars = remove_word_letters_from_chars('moose', remaining_chars)

    words_list = get_words_list_from_words_file()
    ipdb.set_trace()
    words_list = remove_unspellable_words(words_list, remaining_chars)
    words_by_length_dict = get_words_by_length_dict(words_list)
    possible_solutions = init_possible_solutions(possible_solutions, words_by_length_dict)

    ipdb.set_trace()

    possible_solutions, remaining_chars = reduce_possible_solutions(possible_solutions,
            remaining_chars)


if __name__ == '__main__':
    main()
