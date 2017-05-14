
# Data
All data files have been removed from the repository 
```
git filter-branch --force --index-filter \
	'git rm --cached --ignore-unmatch *.csv *.RData *.out *.xlsx' \
	--prune-empty --tag-name-filter cat -- --all
```