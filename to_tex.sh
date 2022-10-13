#!/bin/bash 

slide_begin=$(cat slide_base/slide_begin)
slide_end=$(cat slide_base/slide_end)

export slide_begin
export slide_end

awk '/next-slide/{gsub("\"", ""); print substr($0, 1, length($0)-1)ENVIRON["slide_end"]; next}1' $1 \
    | awk '{sub(/next-slide/,ENVIRON["slide_begin"]); print }' > slides.tex
awk '/begin/{p++;if(p==1){next}}p' slides.tex > temp

cat slide_base/begin temp slide_base/end > slides.tex

rm temp
