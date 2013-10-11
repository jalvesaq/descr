/* This file is part of descr R package
**
** It is distributed under the GNU General Public License.
** See the file ../LICENSE for details.
** 
** (c) 2009-2012 Jakson Aquino: jalvesaq@gmail.com
**
***************************************************************/

#include <R.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("descr", String)
#else
#define _(String) (String)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void realfwf2csv(char **fwffile, char **csvfile, char **names, int *begin,
        int *end, int *ncols, int *verbose){

    int i, j, k, maxvlen = 0, len, l = 0, min, max = 0, maxget, nskipped = 0;
    char *b;
    char *value;
    char *v;
    FILE *fwf, *csv;
    int n = ncols[0];

    /* Convert from R vector to C array */
    for(i = 0; i < n; i++){
        if((end[i] - begin[i]) > maxvlen)
            maxvlen = end[i] - begin[i];
        if(end[i] > max)
            max = end[i];
        begin[i] -= 1;
    }
    max += 3;
    min = max - 3;
    maxget = max * 2;

    value = (char*)malloc((maxvlen + 3) * sizeof(char));
    if(value == NULL){
        REprintf(_("Error: could not allocate memory (%d bytes)\n"), maxvlen + 3 *
                sizeof(char));
        return;
    }

    b = (char*)malloc((maxget + 3) * sizeof(char));
    if(b == NULL){
        REprintf(_("Error: could not allocate memory (%d bytes)\n"), maxget + 3 *
                sizeof(char));
        return;
    }
    fwf = fopen(fwffile[0], "r");
    if(fwf == NULL){
        REprintf(_("Error: could not read file \"%s\".\n"), fwffile[0]);
        return;
    }
    csv = fopen(csvfile[0], "w");
    if(csv == NULL){
        REprintf(_("Error: could not write file \"%s\".\n"), csvfile[0]);
        return;
    }

    /* Put the header in the csv file */
    for(i = 0; i < n; i++){
        if(i < (n - 1))
            fprintf(csv, "%s\t", names[i]);
        else
            fprintf(csv, "%s\n", names[i]);
    }

    /* Put the rows in the csv file */
    while(fgets(b, maxget - 3, fwf)){
        l++;

        /* delete the new line character */
        i = strlen(b) - 1;
        while(i > 0 && (b[i] == '\n' || b[i] == '\r')){
            b[i] = 0;
            i--;
        }

        len = strlen(b);
        if(len < 3){
            nskipped += 1;
            continue;
        }
        if(len < min){
            REprintf(_("Error: line %d has only %d characters.\n"), l, len);
            fclose(csv);
            fclose(fwf);
            return;
        }
        for(i = 0; i < n; i++){
            j = begin[i];
            k = 0;
            while(j < end[i]){
                value[k] = b[j];
                k++;
                j++;
            }
            value[k] = 0;

            /* delete empty spaces at the end of the field */
            k--;
            while(value[k] == ' ' && k > 0){
                value[k] = 0;
                k--;
            }

            /* skip empty spaces at the beginning of the field */
            v = value;
            while(*v == ' ')
                v++;

            /* put the value into the csv file */
            fprintf(csv, "%s", v);

            /* put either a field separator or the end of line */
            if(i == (n - 1))
                putc('\n', csv);
            else
                putc('\t', csv);
        }
    }

    /* Finish */
    fclose(fwf);
    fclose(csv);
    free(value);
    free(b);
    if(verbose[0] == 1)
        REprintf(_("%d lines written in \"%s\".\n"), l, csvfile[0]);
    if(nskipped == 1)
        REprintf(_("One line from \"%s\" skipped because shorter than 3 characters.\n"), fwffile[0]);
    else
        if(nskipped > 0)
            REprintf(_("%d lines from \"%s\" skipped because shorter than 3 characters.\n"),
                    nskipped, fwffile[0]);
}

static int getting_var = 0, n = 0, nexc = 0;

void printRCode(FILE *O, char *levels, char *labels, char *dfname, char *var, char *exclud, char *varlab){
    int i;
    if(getting_var == 1 && n > 0){
        if((n - nexc) > 0){
            i = strlen(levels);
            levels[i-2] = 0;
            i = strlen(labels);
            labels[i-2] = 0;
            if(strlen(exclud) == 0){
                fprintf(O, "%s$%s <- factor(%s$%s, levels=c(%s), labels=c(%s))\n",
                        dfname, var, dfname, var, levels, labels);
            } else{
                i = strlen(exclud);
                exclud[i-2] = 0;
                fprintf(O, "%s$%s <- factor(%s$%s, levels=c(%s), exclude=c(%s), labels=c(%s))\n",
                        dfname, var, dfname, var, levels, exclud, labels);
            }
        } else{
            if(nexc == 1){
                i = strlen(exclud);
                exclud[i-2] = 0;
                fprintf(O, "%s$%s[%s$%s == %s] <- NA\n", dfname, var, dfname, var, exclud);
            } else{
                REprintf("[ %s ] Process nexcl > 1 : Not implemented yet!\n", var);
            }
        }
        if(strlen(varlab) > 0)
            fprintf(O, "attr(%s$%s, \"label\") <- \"%s\"\n", dfname, var, varlab);
    } else {
        if(getting_var == 1 && n == 0){
            if(strlen(varlab) > 0)
                fprintf(O, "attr(%s$%s, \"label\") <- \"%s\"\n", dfname, var, varlab);
        }
    }
    n = 0;
    nexc = 0;
    getting_var = 0;
}

void reallabels2R(char **inout, char **dfname, char **nastr, int *nna){

    int i, k, is_na;

    char b[512];
    char var[512];
    char varlab[512];
    char tlev[512];
    char tlab[512];
    char levels[100000];
    char labels[100000];
    char exclud[100000];
    i = 0;
    char *c;
    FILE *I = fopen(inout[0], "r");
    if(I == NULL){
        REprintf(_("Error: could not read file \"%s\".\n"), inout[0]);
        return;
    }
    FILE *O = fopen(inout[1], "w");
    if(O == NULL){
        REprintf(_("Error: could not write file \"%s\".\n"), inout[1]);
        return;
    }
    while((fgets(b, 511, I))){
        if((b[0] >= 'a' && b[0] <= 'z') || (b[0] >= 'A' && b[0] <= 'Z')){
            memset(var, 0, 512);
            memset(varlab, 0, 512);
            memset(levels, 0, 100000);
            memset(labels, 0, 100000);
            memset(exclud, 0, 100000);
            c = b;
            i = 0;
            while(!(*c == '\n' || *c == ' ')){
                var[i] = *c;
                i++;
                c++;
            }
            var[i] = 0;
            if(*c == ' '){
                while(*c == ' ')
                    c++;
                i = 0;
                while(*c != '\n'){
                    varlab[i] = *c;
                    i++;
                    c++;
                }
                varlab[i] = 0;
            }
            getting_var = 1;
            i = 0;
            n = 0;
            nexc = 0;
        }
        if(b[0] >= '0' && b[0] <= '9'){
            n++;
            c = b;
            i = 0;
            while(*c != ' '){
                tlev[i] = *c; i++;
                c++;
            }
            tlev[i] = ','; i++;
            tlev[i] = ' '; i++;
            tlev[i] = 0;
            c++;
            strncat(levels, tlev, 99999);
            i = 0;
            tlab[i] = '"'; i++;
            while(*c != '\n'){
                tlab[i] = *c; i++;
                c++;
            }
            tlab[i] = '"'; i++;
            tlab[i] = ','; i++;
            tlab[i] = ' '; i++;
            tlab[i] = 0;
            is_na = 0;
            for(k = 0; k < *nna; k++)
                if(strcmp(tlab, nastr[k]) ==0){
                    is_na = 1;
                    nexc++;
                    break;
                }
            if(is_na){
                strncat(exclud, tlev, 99999);
            } else{
                strncat(labels, tlab, 99999);
            }
        }
        if(b[0] == '\n'){
            printRCode(O, levels, labels, *dfname, var, exclud, varlab);
        }
    }
    printRCode(O, levels, labels, *dfname, var, exclud, varlab);
    fclose(I);
    fclose(O);
}

