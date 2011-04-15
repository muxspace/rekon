echo
echo "Begining Install"
echo
curl -L https://github.com/adamhunter/rekon/tarball/master > /tmp/rekon.tar.gz
echo
echo "Download Completed"
echo "Extracting Source"
rm -rf /tmp/extract-rekon /tmp/rekon
mkdir /tmp/extract-rekon /tmp/rekon
tar xzf /tmp/rekon.tar.gz -C /tmp/extract-rekon
mv -f /tmp/extract-rekon/*/* /tmp/rekon/
echo "Installing to Riak"
echo
/tmp/rekon/install.sh
echo
echo "Install Completed!"
open "http://127.0.0.1:8098/riak/rekon/go"
