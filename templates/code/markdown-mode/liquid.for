# -*- mode: snippet -*-
# name  : liquid.for
# uuid  : liquid.for
# key   : liquid.for
# group :
# --
{% for ${1:item} in ${2:array,hash} %}
   {{ $1 }}
{% endfor %}
