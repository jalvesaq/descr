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
