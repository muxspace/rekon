#!/bin/bash
# Author: Brian Lee Yung Rowe
# Date: 2001.07.14
# Rewritten from the original version by Adam Hunter

do_exit()
{
  echo $1
  exit $2
}

do_usage()
{
  do_exit "Usage: $0 [-aejdv] [-E erlang_dir] [host:port]" $1
}

content_type()
{
  case $1 in
    go | *.html) content_type="text/html";;
    *.js) content_type="application/javascript";;
    *.css) content_type="text/css";;
    *.png) content_type="image/png";;
    *.gif) content_type="image/gif";;
    *.template) content_type="application/x-sammy-template";;
    *) content_type="text/plain";;
  esac
  echo $content_type
}

do_install_rekon()
{
  echo "Installing rekon to $node..."
  riak_url="http://$node/riak/rekon"
  base_dir="`dirname $0`/app"

  for f in $(ls $base_dir); do
    [ -n "$verbose" ] && echo "Uploading $f to rekon"
    content_type=$(content_type $f)
    curl -X PUT -H"Content-Type: $content_type" $riak_url/$f --data-binary @$base_dir/$f
  done
}

do_install_jobs()
{
  echo "Installing jobs"
  riak_url="http://$node/riak/rekon.jobs"
  base_dir="`dirname $0`/jobs"
  for f in $(ls $base_dir)
  do
    [ -n "$verbose" ] && echo "Uploading job $f to rekon.jobs"
    content_type="application/javascript"
    curl -X PUT -H"Content-Type: $content_type" $riak_url/$f --data-binary @$base_dir/$f
  done
}

do_install_data()
{
  echo "Installing data"
  riak_url="http://$node/riak"
  base_dir=$(dirname $0)/buckets
  for bucket in $(ls $base_dir)
  do
    dir=$base_dir/$bucket
    [ -n "$verbose" ] && echo "Adding bucket $bucket"
    for f in $(ls $dir)
    do
      [ -n "$verbose" ] && echo "Adding file as $bucket/$f"
      content_type=$(content_type $f)
      curl -X PUT -H"Content-Type: $content_type" $riak_url/$bucket/$f --data-binary @$dir/$f
    done
  done
}

do_install_erlang()
{
  echo "Installing erlang modules to $module_dir"
  sudo mkdir -p $module_dir
  sudo erlc -o $module_dir erlang/*.erl

}

node="127.0.0.1:8098"
module_dir=/etc/riak/erlang
while getopts "aeE:jdv?" option
do
  case $option in
    a) erlang=yes; jobs=yes; data=yes;;
    e) erlang=yes;;
    E) erlang=yes; module_dir=$OPTARG;;
    j) jobs=yes;;
    d) data=yes;;
    v) verbose=yes;;
    '?') do_usage 0;;
    *) do_usage 1;;
  esac
done
shift $(($OPTIND - 1))

[ -n "$1" ] && node=$1

do_install_rekon
[ -n "$jobs" ] && do_install_jobs
[ -n "$data" ] && do_install_data
[ -n "$erlang" ] && do_install_erlang


echo "Installed, now visit: $riak_url/go"
