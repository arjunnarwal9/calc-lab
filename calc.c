// Arjun  251ADB007
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>

#define STUDENT_NAME "arjun"
#define STUDENT_LASTNAME "narwal"
#define STUDENT_ID "251ADB007"

/* Keeping the original integer alongside its double value lets the parser
   preserve C-style integer division unless either operand is a float. */
typedef struct {
    int is_float;
    long long integer;
    double real;
} Value;

typedef struct {
    const char *text;
    size_t length;
    size_t pos;
    size_t line_end;
    size_t error_pos;
    int failed;
} Parser;

static Value make_integer(long long n)
{
    Value v;
    v.is_float = 0;
    v.integer = n;
    v.real = (double)n;
    return v;
}

static Value make_float(double n)
{
    Value v;
    v.is_float = 1;
    v.integer = 0;
    v.real = n;
    return v;
}

/* Positions are offsets in the original file, not in a copied line. That is
   why the parser can report the exact 1-based position even after comments
   and earlier lines have been skipped. */
static void set_error(Parser *p, size_t position)
{
    if (!p->failed) {
        p->failed = 1;
        p->error_pos = position;
    }
}

static void skip_spaces(Parser *p)
{
    while (p->pos < p->line_end &&
           (p->text[p->pos] == ' ' || p->text[p->pos] == '\t')) {
        p->pos++;
    }
}

static size_t current_position(Parser *p)
{
    skip_spaces(p);
    if (p->pos >= p->line_end) {
        /* A missing operand or closing parenthesis belongs to the newline;
           for a final line without newline, its position is file length + 1. */
        return p->line_end + 1;
    }
    return p->pos + 1;
}

static Value parse_expression(Parser *p);
static Value parse_power(Parser *p);

static Value parse_primary(Parser *p)
{
    Value value = make_integer(0);
    skip_spaces(p);

    if (p->pos >= p->line_end) {
        set_error(p, p->line_end + 1);
        return value;
    }

    char c = p->text[p->pos];
    if (c == '(') {
        /* If the closing bracket is absent, the parser has reached the line
           boundary, so the newline (or EOF + 1) is the reported position. */
        p->pos++;
        value = parse_expression(p);
        if (p->failed) return value;
        skip_spaces(p);
        if (p->pos >= p->line_end) {
            set_error(p, p->line_end + 1);
            return value;
        }
        if (p->text[p->pos] != ')') {
            set_error(p, p->pos + 1);
            return value;
        }
        p->pos++;
        return value;
    }

    if (isdigit((unsigned char)c)) {
        long long integer = 0;
        double real = 0.0;
        int has_dot = 0;
        int digits_after_dot = 0;
        double fraction = 0.0;

        while (p->pos < p->line_end &&
               isdigit((unsigned char)p->text[p->pos])) {
            int digit = p->text[p->pos] - '0';
            integer = integer * 10 + digit;
            real = real * 10.0 + (double)digit;
            p->pos++;
        }

        if (p->pos < p->line_end && p->text[p->pos] == '.') {
            has_dot = 1;
            p->pos++;
            if (p->pos >= p->line_end ||
                !isdigit((unsigned char)p->text[p->pos])) {
                set_error(p, p->pos - 1 + 1);
                return value;
            }
            while (p->pos < p->line_end &&
                   isdigit((unsigned char)p->text[p->pos])) {
                int digit = p->text[p->pos] - '0';
                fraction += (double)digit / pow(10.0, (double)(digits_after_dot + 1));
                digits_after_dot++;
                p->pos++;
            }
        }

        if (has_dot) {
            value = make_float(real + fraction);
        } else {
            value = make_integer(integer);
        }
        return value;
    }

    /* Unary operators are deliberately not accepted. Any character that
       cannot begin a primary is the first token where parsing fails. */
    set_error(p, p->pos + 1);
    return value;
}

static Value apply_power(Parser *p, Value base, Value exponent,
                         size_t operator_pos, size_t exponent_pos)
{
    if (!base.is_float && !exponent.is_float) {
        if (exponent.integer < 0) {
            set_error(p, exponent_pos);
            return make_integer(0);
        }
        long long result = 1;
        long long factor = base.integer;
        unsigned long long power = (unsigned long long)exponent.integer;
        while (power > 0) {
            if (power & 1ULL) result *= factor;
            power >>= 1;
            if (power > 0) factor *= factor;
        }
        return make_integer(result);
    }

    double result = pow(base.real, exponent.real);
    if (!isfinite(result)) {
        set_error(p, operator_pos);
        return make_integer(0);
    }
    return make_float(result);
}

/* The exponent is parsed by another power() call, rather than a loop. This
   makes 2**3**2 become 2**(3**2), while term() keeps * and / left-associative. */
static Value parse_power(Parser *p)
{
    Value base = parse_primary(p);
    if (p->failed) return base;

    skip_spaces(p);
    if (p->pos + 1 < p->line_end && p->text[p->pos] == '*' &&
        p->text[p->pos + 1] == '*') {
        size_t operator_pos = p->pos + 1;
        p->pos += 2;
        size_t exponent_pos = current_position(p);
        Value exponent = parse_power(p);
        if (p->failed) return exponent;
        return apply_power(p, base, exponent, operator_pos, exponent_pos);
    }
    return base;
}

static Value parse_term(Parser *p)
{
    Value left = parse_power(p);
    while (!p->failed) {
        skip_spaces(p);
        if (p->pos >= p->line_end) break;
        char op = p->text[p->pos];
        if (op != '*' && op != '/') break;
        /* A pair of stars is exponentiation, not multiplication. */
        if (op == '*' && p->pos + 1 < p->line_end &&
            p->text[p->pos + 1] == '*') break;

        size_t operator_pos = p->pos + 1;
        p->pos++;
        size_t right_pos = current_position(p);
        Value right = parse_power(p);
        if (p->failed) return left;

        /* Division-by-zero points to the start of the right operand, not to
           the slash, as required by the assignment. */
        if (op == '/') {
            if ((right.is_float && right.real == 0.0) ||
                (!right.is_float && right.integer == 0)) {
                set_error(p, right_pos);
                return left;
            }
            if (left.is_float || right.is_float) {
                double result = left.real / right.real;
                if (!isfinite(result)) {
                    set_error(p, operator_pos);
                    return left;
                }
                left = make_float(result);
            } else {
                left = make_integer(left.integer / right.integer);
            }
        } else if (left.is_float || right.is_float) {
            double result = left.real * right.real;
            if (!isfinite(result)) {
                set_error(p, operator_pos);
                return left;
            }
            left = make_float(result);
        } else {
            left = make_integer(left.integer * right.integer);
        }
    }
    return left;
}

static Value parse_expression(Parser *p)
{
    Value left = parse_term(p);
    while (!p->failed) {
        skip_spaces(p);
        if (p->pos >= p->line_end) break;
        char op = p->text[p->pos];
        if (op != '+' && op != '-') break;

        size_t operator_pos = p->pos + 1;
        p->pos++;
        Value right = parse_term(p);
        if (p->failed) return left;

        if (left.is_float || right.is_float) {
            double result = (op == '+') ? left.real + right.real
                                        : left.real - right.real;
            if (!isfinite(result)) {
                set_error(p, operator_pos);
                return left;
            }
            left = make_float(result);
        } else if (op == '+') {
            left = make_integer(left.integer + right.integer);
        } else {
            left = make_integer(left.integer - right.integer);
        }
    }
    return left;
}

static int evaluate_file(const char *input_path, const char *output_path)
{
    FILE *input = fopen(input_path, "rb");
    if (input == NULL) {
        fprintf(stderr, "Cannot open input file '%s': %s\n", input_path, strerror(errno));
        return -1;
    }

    size_t capacity = 8192;
    size_t length = 0;
    char *data = malloc(capacity);
    if (data == NULL) {
        fprintf(stderr, "Memory allocation failed.\n");
        fclose(input);
        return -1;
    }

    int ch;
    while ((ch = fgetc(input)) != EOF) {
        if (length == capacity) {
            if (capacity > (size_t)-1 / 2) {
                fprintf(stderr, "Input file is too large.\n");
                free(data);
                fclose(input);
                return -1;
            }
            size_t new_capacity = capacity * 2;
            char *new_data = realloc(data, new_capacity);
            if (new_data == NULL) {
                fprintf(stderr, "Memory allocation failed.\n");
                free(data);
                fclose(input);
                return -1;
            }
            data = new_data;
            capacity = new_capacity;
        }
        data[length++] = (char)ch;
    }
    if (ferror(input)) {
        fprintf(stderr, "Error reading '%s'.\n", input_path);
        free(data);
        fclose(input);
        return -1;
    }
    if (fclose(input) != 0) {
        fprintf(stderr, "Error closing '%s'.\n", input_path);
        free(data);
        return -1;
    }

    FILE *output = fopen(output_path, "w");
    if (output == NULL) {
        fprintf(stderr, "Cannot write output file '%s': %s\n", output_path, strerror(errno));
        free(data);
        return -1;
    }

    /* Parse one line at a time, but keep every cursor as an offset into data.
       The buffer grows geometrically, avoiding repeated copying on large files. */
    size_t line_start = 0;
    size_t error_position = 0;
    char result_line[128];
    char **results = NULL;
    size_t result_count = 0;
    size_t result_capacity = 0;

    while (line_start < length) {
        size_t line_end = line_start;
        while (line_end < length && data[line_end] != '\n') line_end++;

        size_t first = line_start;
        while (first < line_end && (data[first] == ' ' || data[first] == '\t' || data[first] == '\r')) first++;

        if (first < line_end && data[first] != '#') {
            Parser parser;
            parser.text = data;
            parser.length = length;
            parser.pos = line_start;
            parser.line_end = line_end;
            parser.error_pos = 0;
            parser.failed = 0;

            Value value = parse_expression(&parser);
            if (!parser.failed) {
                skip_spaces(&parser);
                if (parser.pos < line_end) set_error(&parser, parser.pos + 1);
            }

            if (parser.failed) {
                error_position = parser.error_pos;
                break;
            }

            if (value.is_float) {
                snprintf(result_line, sizeof(result_line), "%.15g", value.real);
            } else {
                snprintf(result_line, sizeof(result_line), "%lld", value.integer);
            }

            if (result_count == result_capacity) {
                size_t new_capacity = result_capacity == 0 ? 16 : result_capacity * 2;
                char **new_results = realloc(results, new_capacity * sizeof(*results));
                if (new_results == NULL) {
                    fprintf(stderr, "Memory allocation failed.\n");
                    for (size_t i = 0; i < result_count; i++) free(results[i]);
                    free(results);
                    free(data);
                    fclose(output);
                    return -1;
                }
                results = new_results;
                result_capacity = new_capacity;
            }
            size_t result_length = strlen(result_line) + 1;
            results[result_count] = malloc(result_length);
            if (results[result_count] == NULL) {
                fprintf(stderr, "Memory allocation failed.\n");
                for (size_t i = 0; i < result_count; i++) free(results[i]);
                free(results);
                free(data);
                fclose(output);
                return -1;
            }
            memcpy(results[result_count++], result_line, result_length);
        }

        if (line_end == length) {
            line_start = length;
        } else {
            line_start = line_end + 1;
        }
    }

    /* A trailing newline does not create another expression. A non-newline
       final line is handled by the loop above, with EOF errors at length + 1. */
    if (error_position != 0) {
        if (fprintf(output, "ERROR:%zu\n", error_position) < 0) {
            fprintf(stderr, "Error writing '%s'.\n", output_path);
            for (size_t i = 0; i < result_count; i++) free(results[i]);
            free(results);
            free(data);
            fclose(output);
            return -1;
        }
    } else {
        for (size_t i = 0; i < result_count; i++) {
            if (fprintf(output, "%s\n", results[i]) < 0) {
                fprintf(stderr, "Error writing '%s'.\n", output_path);
                for (size_t j = 0; j < result_count; j++) free(results[j]);
                free(results);
                free(data);
                fclose(output);
                return -1;
            }
        }
    }

    for (size_t i = 0; i < result_count; i++) free(results[i]);
    free(results);
    free(data);
    if (fclose(output) != 0) {
        fprintf(stderr, "Error closing output file '%s'.\n", output_path);
        return -1;
    }
    return 0;
}

static const char *base_name(const char *path)
{
    const char *slash = strrchr(path, '/');
    return slash == NULL ? path : slash + 1;
}

static char *input_base(const char *path)
{
    const char *base = base_name(path);
    size_t n = strlen(base);
    if (n >= 4 && strcmp(base + n - 4, ".txt") == 0) n -= 4;
    char *result = malloc(n + 1);
    if (result == NULL) return NULL;
    memcpy(result, base, n);
    result[n] = '\0';
    return result;
}

static char *join_path(const char *left, const char *right)
{
    size_t a = strlen(left);
    size_t b = strlen(right);
    int slash_needed = a > 0 && left[a - 1] != '/';
    char *result = malloc(a + (size_t)slash_needed + b + 1);
    if (result == NULL) return NULL;
    memcpy(result, left, a);
    if (slash_needed) result[a++] = '/';
    memcpy(result + a, right, b + 1);
    return result;
}

static int ensure_output_dir(const char *path)
{
    struct stat info;
    if (stat(path, &info) == 0) {
        if (!S_ISDIR(info.st_mode)) {
            fprintf(stderr, "Output path '%s' is not a directory.\n", path);
            return -1;
        }
        return 0;
    }
    if (errno != ENOENT) {
        fprintf(stderr, "Cannot access output directory '%s': %s\n", path, strerror(errno));
        return -1;
    }
    if (mkdir(path, 0777) != 0) {
        fprintf(stderr, "Cannot create output directory '%s': %s\n", path, strerror(errno));
        return -1;
    }
    return 0;
}

static char *make_output_filename(const char *outdir, const char *input_path)
{
    char *base = input_base(input_path);
    if (base == NULL) return NULL;
    size_t needed = strlen(base) + strlen(STUDENT_NAME) + strlen(STUDENT_LASTNAME) + strlen(STUDENT_ID) + 8;
    char *filename = malloc(needed);
    if (filename == NULL) {
        free(base);
        return NULL;
    }
    snprintf(filename, needed, "%s_%s_%s_%s.txt", base, STUDENT_NAME,
             STUDENT_LASTNAME, STUDENT_ID);
    char *path = join_path(outdir, filename);
    free(filename);
    free(base);
    return path;
}

static int has_txt_extension(const char *name)
{
    size_t n = strlen(name);
    return n >= 4 && strcmp(name + n - 4, ".txt") == 0;
}

static int process_directory(const char *directory, const char *outdir)
{
    DIR *dir = opendir(directory);
    if (dir == NULL) {
        fprintf(stderr, "Cannot open input directory '%s': %s\n", directory, strerror(errno));
        return -1;
    }

    int status = 0;
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (entry->d_name[0] == '.' || !has_txt_extension(entry->d_name)) continue;
        char *input_path = join_path(directory, entry->d_name);
        if (input_path == NULL) {
            fprintf(stderr, "Memory allocation failed.\n");
            status = -1;
            break;
        }
        struct stat info;
        if (stat(input_path, &info) != 0 || !S_ISREG(info.st_mode)) {
            free(input_path);
            continue;
        }
        char *output_path = make_output_filename(outdir, input_path);
        if (output_path == NULL) {
            fprintf(stderr, "Memory allocation failed.\n");
            free(input_path);
            status = -1;
            break;
        }
        if (evaluate_file(input_path, output_path) != 0) status = -1;
        free(output_path);
        free(input_path);
        if (status != 0) break;
    }

    if (closedir(dir) != 0) {
        fprintf(stderr, "Error closing input directory '%s'.\n", directory);
        status = -1;
    }
    return status;
}

int main(int argc, char *argv[])
{
    const char *directory = NULL;
    const char *output_option = NULL;
    const char *input_file = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (directory != NULL || input_file != NULL || i + 1 >= argc) {
                fprintf(stderr, "Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
                return EXIT_FAILURE;
            }
            directory = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (output_option != NULL || i + 1 >= argc) {
                fprintf(stderr, "Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
                return EXIT_FAILURE;
            }
            output_option = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return EXIT_FAILURE;
        } else {
            if (input_file != NULL || directory != NULL) {
                fprintf(stderr, "Provide either one input file or one input directory.\n");
                return EXIT_FAILURE;
            }
            input_file = argv[i];
        }
    }

    if ((directory == NULL && input_file == NULL) ||
        (directory != NULL && input_file != NULL)) {
        fprintf(stderr, "Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
        return EXIT_FAILURE;
    }

    char *default_outdir = NULL;
    const char *outdir = output_option;
    if (outdir == NULL) {
        char *base = input_base(directory != NULL ? directory : input_file);
        if (base == NULL) {
            fprintf(stderr, "Memory allocation failed.\n");
            return EXIT_FAILURE;
        }
        size_t needed = strlen(base) + strlen(STUDENT_NAME) + strlen(STUDENT_ID) + 3;
        default_outdir = malloc(needed);
        if (default_outdir == NULL) {
            free(base);
            fprintf(stderr, "Memory allocation failed.\n");
            return EXIT_FAILURE;
        }
        snprintf(default_outdir, needed, "%s_%s_%s", base, STUDENT_NAME, STUDENT_ID);
        free(base);
        outdir = default_outdir;
    }

    if (ensure_output_dir(outdir) != 0) {
        free(default_outdir);
        return EXIT_FAILURE;
    }

    int status;
    if (directory != NULL) {
        status = process_directory(directory, outdir);
    } else {
        char *output_path = make_output_filename(outdir, input_file);
        if (output_path == NULL) {
            fprintf(stderr, "Memory allocation failed.\n");
            free(default_outdir);
            return EXIT_FAILURE;
        }
        status = evaluate_file(input_file, output_path);
        free(output_path);
    }

    free(default_outdir);
    return status == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
