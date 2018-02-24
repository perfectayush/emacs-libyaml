/*
  Copyright (C) 2016 by Syohei YOSHIDA

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <emacs-module.h>
#include <yaml.h>

int plugin_is_GPL_compatible;

static bool
is_null_str(const char *s, ptrdiff_t len)
{
	switch (len) {
	case 1:
		return (s[0] == '\0');
	case 3:
		return (strcmp("nil", s) == 0);
	case 4:
		return (strcmp("null", s) == 0 || strcmp("Null", s) == 0
			|| strcmp("NULL", s) == 0);
	default:
		return false;
	}
}

static bool
is_true_str(const char *s, ptrdiff_t len)
{
	switch (len) {
	case 1:
		return (s[0] == 't' || s[0] == 'T');
	case 2:
		return (strcmp("on", s) == 0 || strcmp("On", s) == 0
			|| strcmp("ON", s) == 0);
	case 3:
		return (strcmp("yes", s) == 0 || strcmp("Yes", s) == 0
			|| strcmp("YES", s) == 0);
	case 4:
		return (strcmp("true", s) == 0 || strcmp("True", s) == 0
			|| strcmp("TRUE", s) == 0);
	default:
		return false;
	}
}

static bool
is_false_str(const char *s, ptrdiff_t len)
{
	switch (len) {
	case 1:
		return (s[0] == 'n' || s[0] == 'N');
	case 2:
		return (strcmp("no", s) == 0 || strcmp("No", s) == 0
			|| strcmp("NO", s) == 0);
	case 3:
		return (strcmp("off", s) == 0 || strcmp("Off", s) == 0
			|| strcmp("OFF", s) == 0);
	case 5:
		return (strcmp("false", s) == 0 || strcmp("False", s) == 0
			|| strcmp("FALSE", s) == 0);
	default:
		return false;
	}
}

static emacs_value
yaml_node_to_elisp(emacs_env *env, yaml_document_t *document, yaml_node_t *node)
{
	if (node == NULL)
		return env->intern(env, "nil");

	switch (node->type) {
	case YAML_SCALAR_NODE: {
		const char *str = (const char*)node->data.scalar.value;
		ptrdiff_t str_len = node->data.scalar.length;

		if (node->data.scalar.style != YAML_PLAIN_SCALAR_STYLE) {
			return env->make_string(env, str, str_len);
		}

		if (str[0] == '~')
			return env->intern(env, "nil");

		if (is_null_str(str, str_len) || is_false_str(str, str_len)) {
			return env->intern(env, "nil");
		} else if (is_true_str(str, str_len)) {
			return env->intern(env, "t");
		}

		char *endptr;
		long long ll = strtoll(str, &endptr, 0);
		if (str != endptr && *endptr == '\0') {
			return env->make_integer(env, (intmax_t)ll);
		}

		double d = strtod(str, &endptr);
		if (str != endptr && *endptr == '\0') {
			return env->make_float(env, d);
		}

		return env->make_string(env, str, str_len);
	}
	case YAML_SEQUENCE_NODE: {
		yaml_node_item_t *beg = node->data.sequence.items.start;
		yaml_node_item_t *end = node->data.sequence.items.top;
		ptrdiff_t len = end - beg;

		if (len == 0)
			return env->intern(env, "nil");

		emacs_value *array = malloc(sizeof(emacs_value) * len);
		if (array == NULL)
			return env->intern(env, "nil");

		size_t i = 0;
		for (yaml_node_item_t *item = beg; item < end; ++item, ++i) {
			yaml_node_t *elm = yaml_document_get_node(document, *item);
			array[i] = yaml_node_to_elisp(env, document, elm);
		}

		emacs_value Fvector = env->intern(env, "vector");
		emacs_value vec = env->funcall(env, Fvector, len, array);
		free(array);
		return vec;

	}
	case YAML_MAPPING_NODE: {
		emacs_value Fmake_hash_table = env->intern(env, "make-hash-table");
		emacs_value make_hash_args[] = {env->intern(env, ":test"),
						env->intern(env, "equal")};
		emacs_value hash = env->funcall(env, Fmake_hash_table, 2, make_hash_args);
		emacs_value Fputhash = env->intern(env, "puthash");

		yaml_node_pair_t *beg = node->data.mapping.pairs.start;
		yaml_node_pair_t *end = node->data.mapping.pairs.top;

		for (yaml_node_pair_t *pair = beg; pair < end; ++pair) {
			yaml_node_t *key = yaml_document_get_node(document, pair->key);
			yaml_node_t *val = yaml_document_get_node(document, pair->value);

			emacs_value key_lisp = yaml_node_to_elisp(env, document, key);
			emacs_value val_lisp = yaml_node_to_elisp(env, document, val);

			emacs_value puthash_args[] = {key_lisp, val_lisp, hash};
			env->funcall(env, Fputhash, 3, puthash_args);
		}

		return hash;
	}
	default:
		return env->intern(env, "nil");
	}
}

static emacs_value
Fyaml_load(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	emacs_value code = args[0];
	ptrdiff_t size = 0;
	char *yaml_str = NULL;

	env->copy_string_contents(env, code, NULL, &size);
	yaml_str = malloc(size);
	if (yaml_str == NULL)
		return env->intern (env, "nil");
	env->copy_string_contents(env, code, yaml_str, &size);

	yaml_parser_t parser;
	yaml_parser_initialize(&parser);
	yaml_parser_set_input_string(&parser, (unsigned char*)yaml_str, size-1);

	yaml_document_t document;
	yaml_parser_load(&parser, &document);
	if (parser.error != YAML_NO_ERROR) {
		return env->intern(env, "nil");
	}

	yaml_node_t *root = yaml_document_get_root_node(&document);
	return yaml_node_to_elisp(env, &document, root);
}

static bool
eq_type(emacs_env *env, emacs_value type, const char *type_str)
{
	return env->eq(env, type, env->intern(env, type_str));
}

static emacs_value
hash_keys(emacs_env *env, emacs_value hash)
{
	emacs_value Qhash_table_keys = env->intern(env, "hash-table-keys");
	emacs_value args[1] = { hash };

	return env->funcall(env, Qhash_table_keys, 1, args);
}

static emacs_value
hash_value(emacs_env *env, emacs_value hash, emacs_value key)
{
	emacs_value Qgethash = env->intern(env, "gethash");
	emacs_value args[2] = { key, hash };

	return env->funcall(env, Qgethash, 2, args);
}

static intmax_t
list_length(emacs_env *env, emacs_value seq)
{
	emacs_value Qlength = env->intern(env, "length");
	emacs_value args[1] = { seq };

	emacs_value len = env->funcall(env, Qlength, 1, args);
	return env->extract_integer(env, len);
}

static emacs_value
list_nth(emacs_env *env, emacs_value seq, intmax_t index)
{
	emacs_value Qnth = env->intern(env, "nth");
	emacs_value i = env->make_integer(env, index);
	emacs_value args[2] = { i, seq };

	return env->funcall(env, Qnth, 2, args);
}

static int
emacs_value_to_yaml(emacs_env *env, yaml_document_t *document, emacs_value value)
{
	emacs_value type = env->type_of(env, value);

	if (eq_type(env, type, "vector")) {
		emacs_value vec = value;
		intmax_t len = env->vec_size(env, vec);

		int node = yaml_document_add_sequence(document, NULL, YAML_ANY_SEQUENCE_STYLE);

		for (ptrdiff_t i = 0; i < len; ++i) {
			int child_node = emacs_value_to_yaml(env, document, env->vec_get(env, vec, i));

			yaml_document_append_sequence_item(document, node, child_node);
		}

		return node;
	} else if (eq_type(env, type, "hash-table")) {
		emacs_value hash = value;
		emacs_value keys = hash_keys(env, hash);
		intmax_t size = list_length(env, keys);

		int node = yaml_document_add_mapping(document, NULL, YAML_ANY_MAPPING_STYLE);
		for (intmax_t i = 0; i < size; ++i) {
			emacs_value k = list_nth(env, keys, i);
			emacs_value v = hash_value(env, hash, k);

			int key_node = emacs_value_to_yaml(env, document, k);
			int val_node = emacs_value_to_yaml(env, document, v);

			yaml_document_append_mapping_pair(document, node,
							  key_node, val_node);
		}

		return node;
	} else {
		if (!eq_type(env, type, "string")) {
			if (env->is_not_nil(env, value)) {
				emacs_value Qformat = env->intern(env, "format");
				emacs_value fmt_args[] = {
					env->make_string(env, "%s", 2), value,
				};

				value = env->funcall(env, Qformat, 2, fmt_args);
			} else {
				value = env->make_string(env, "~", 1);
			}
		}

		yaml_scalar_style_t style = YAML_ANY_MAPPING_STYLE;
		if (list_length(env, value) == 0) {
			style = YAML_SINGLE_QUOTED_SCALAR_STYLE;
		}

		ptrdiff_t len = 0;
		env->copy_string_contents(env, value, NULL, &len);
		char *str_buf = malloc(len);
		env->copy_string_contents(env, value, str_buf, &len);

		yaml_char_t *p = (yaml_char_t*)str_buf;
		int node = yaml_document_add_scalar(document, NULL, p, len-1, style);
		free(str_buf);

		return node;
	}
}

/**
 * @brief ypath_find_index_in_doc finds the index of ypath element in yaml document
 *
 * This function takes 3 parameters, one for yaml document, one for root node and one for yaml path.
 * Recursive calls are made to tokenize yamlpath and search the path in the document
 *
 * For a document like shownin document below, you can find index of key4 with ypath 'key3.key4'.
 * You can also jump to values like value4 using yaml path 'key3.key4.value4'.
 *
 * @param document   Yaml document to be parsed. This is a struct created from yaml_parse_load function
 * @param node       Yaml node to start the search from. By default we pass the root node.
 * @param searchpath Yaml path to be searched for. This is a dot seperated string for key lookup
 * @return Index position of key looked up in the the yaml c string, assuming string index starts from 0.
           Returns -1 if not found
 *
 * @see Fypath_search
 */

static int
ypath_find_index_in_doc(yaml_document_t *document, yaml_node_t *node, char *searchpath)
{
  char searchpath_copy[256];
  strcpy(searchpath_copy, searchpath);
  char *first = strtok(searchpath_copy,".");
  char *rest = strchr(searchpath,'.');
  if(rest != NULL){
    rest++;
  }

  if (first == NULL)
    return node->start_mark.index;

  switch (node->type) {
  case YAML_SCALAR_NODE: {
    if (strcmp(first,(const char*)node->data.scalar.value) == 0 && rest == NULL)
      return node->start_mark.index;
    else
      return -1;
  }
  case YAML_SEQUENCE_NODE: {
    yaml_node_item_t *beg = node->data.sequence.items.start;
    yaml_node_item_t *end = node->data.sequence.items.top;
    ptrdiff_t len = end - beg;
    if (len == 0)
      return -1;
    for (yaml_node_item_t *item = beg; item < end; ++item) {
      yaml_node_t *elm = yaml_document_get_node(document, *item);
      int value = ypath_find_index_in_doc(document, elm, searchpath);
      if (value != -1)
        return value;
    }
  }
  case YAML_MAPPING_NODE: {
    yaml_node_pair_t *beg = node->data.mapping.pairs.start;
    yaml_node_pair_t *end = node->data.mapping.pairs.top;

    for (yaml_node_pair_t *pair = beg; pair < end; ++pair) {
      yaml_node_t *key = yaml_document_get_node(document, pair->key);
      yaml_node_t *val = yaml_document_get_node(document, pair->value);
      if (strcmp(first,(const char *) key->data.scalar.value) == 0){
        if (rest != NULL)
          return ypath_find_index_in_doc(document, val, rest);
        else
          return key->start_mark.index;
      }
    }
  }
  default:
    return -1;
  }
}

/**
 * @brief Fypath_search is the function that we bind to libyaml-ypath module function
 *
 * The declaration is similar to how emacs dynamic module functions are defined in c
 * This function setups a yaml parser and parses the yaml document. The yaml_path
 * is then searched in the yaml document using ypath_find_index_in_doc function and
 * the correct index for character is returned. Index is incremented by 1 as we
 * receive index of C string.
 *
 * @see ypath_find_index_in_doc
 */
static emacs_value
Fypath_search(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	emacs_value code = args[0];
	emacs_value ypath = args[1];
	ptrdiff_t yaml_size = 0;
	ptrdiff_t ypath_size = 0;
	char *yaml_str = NULL;
	char *ypath_str = NULL;

	env->copy_string_contents(env, code, NULL, &yaml_size);
	yaml_str = malloc(yaml_size);
	if (yaml_str == NULL)
		return env->intern (env, "nil");
	env->copy_string_contents(env, code, yaml_str, &yaml_size);


	env->copy_string_contents(env, ypath, NULL, &ypath_size);
	ypath_str = malloc(ypath_size);
	if (ypath_str == NULL)
		return env->intern (env, "nil");
	env->copy_string_contents(env, ypath, ypath_str, &ypath_size);

	yaml_parser_t parser;
	yaml_parser_initialize(&parser);
	yaml_parser_set_input_string(&parser, (unsigned char*)yaml_str, yaml_size-1);

	yaml_document_t document;
	yaml_parser_load(&parser, &document);
	if (parser.error != YAML_NO_ERROR) {
		return env->intern(env, "nil");
	}

	yaml_node_t *root = yaml_document_get_root_node(&document);
	int index = ypath_find_index_in_doc(&document, root, ypath_str);
	return env->make_integer(env, (intmax_t) ++index);
}

struct emacs_write_data_t {
	emacs_env *env;
	emacs_value str;
};

static int
yaml_write_handler(void *data, unsigned char *buf, size_t size)
{
	struct emacs_write_data_t *d = (struct emacs_write_data_t*)data;
	emacs_env *env = d->env;

	emacs_value s = env->make_string(env, (char*)buf, size-1);
	emacs_value Qconcat = env->intern(env, "concat");
	emacs_value concat_args[] = {d->str, s};
	emacs_value new = env->funcall(env, Qconcat, 2, concat_args);
	d->str = new;

	return 1;
}

static emacs_value
Fyaml_dump(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	yaml_document_t document;
	yaml_document_initialize(&document, NULL, NULL, NULL, 0, 0);

	emacs_value root = args[0];
	emacs_value_to_yaml(env, &document, root);

	yaml_emitter_t emitter;
	yaml_emitter_initialize(&emitter);

	struct emacs_write_data_t write_data;
	write_data.env = env;
	write_data.str = env->make_string(env, "", 0);

	yaml_emitter_set_output(&emitter, &yaml_write_handler, &write_data);

	yaml_emitter_open(&emitter);
	yaml_emitter_dump(&emitter, &document);
	yaml_emitter_close(&emitter);

	yaml_emitter_delete(&emitter);
	yaml_document_delete(&document);

	return write_data.str;
}

static void
bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
	emacs_value Qfset = env->intern(env, "fset");
	emacs_value Qsym = env->intern(env, name);
	emacs_value args[] = { Qsym, Sfun };

	env->funcall(env, Qfset, 2, args);
}

static void
provide(emacs_env *env, const char *feature)
{
	emacs_value Qfeat = env->intern(env, feature);
	emacs_value Qprovide = env->intern (env, "provide");
	emacs_value args[] = { Qfeat };

	env->funcall(env, Qprovide, 1, args);
}

int
emacs_module_init(struct emacs_runtime *ert)
{
	emacs_env *env = ert->get_environment(ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) \
	bind_function(env, lsym, env->make_function(env, amin, amax, csym, doc, data))

	DEFUN("libyaml-load", Fyaml_load, 1, 1, "Load YAML", NULL);
	DEFUN("libyaml-dump", Fyaml_dump, 1, 1, "Dump YAML", NULL);
  /**
   * @brief libyaml-ypath is an emacs module function to find character position
   * of a yaml element in a yaml document
   *
   * This function takes 2 parameters, one for yaml string and one for yaml path.
   * yaml path is a simple key lookup in the YAML document. Example usage in elisp
   * @code{.el}
   * (libyaml-ypath yaml_str yaml_path)
   * @code
   *
   * For a document like shownin document below, you can find index of key4 with ypath 'key3.key4'.
   * You can also jump to values like value4 using yaml path 'key3.key4.value4'.
   * @code{.yml}
   * ---
   * key1: value1
   * key2: value2
   * key3:
   *    - value3
   *		- key4: value4
   * @endcode
   *
   * @param yaml_str   Yaml string to be parsed
   * @param yaml_path  Yaml path to be searched for. This is a dot seperated key string
   * @return Index position of key looked up in the the yaml string, assuming index starts from 1.
   *         Returns 0 if not found
   *
   * @see Fypath_search
   * @see ypath_find_index_in_doc
   */
	DEFUN("libyaml-ypath", Fypath_search, 2, 2, "Search ypath in YAML", NULL);

#undef DEFUN

	provide(env, "libyaml-core");
	return 0;
}

/*
  Local Variables:
  c-basic-offset: 8
  indent-tabs-mode: t
  End:
*/
