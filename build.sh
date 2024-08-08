#!/bin/bash

main() {
	usage='Usage: ./build.sh [--autobuild] [-M MODE] [arg ...]'

	autobuild=0
	mode=html
	args=()
	while [[ $# -gt 0 ]]; do
		case $1 in
			--help) echo "$usage"; return 0 ;;
			--autobuild) autobuild=1 ;;
			-M) shift; mode=$1 ;;
			*) args+=("$1") ;;
		esac
		shift
	done

	srcdir=${PWD}
	venv="${srcdir}/_venv"
	builddir="${srcdir}/_build"

	if [[ -z "${IN_NIX_SHELL}" ]] && [[ ! -d "${venv}" ]]; then
		python3 -m venv "${venv}"
		"${venv}/bin/pip" install sphinx sphinx-autobuild
	fi

	if [[ "${autobuild}" == 1 ]]; then
		exec "${venv}/bin/sphinx-autobuild" "${srcdir}" "${builddir}/html" "${args[@]}"
	else
		exec "${venv}/bin/sphinx-build" -M "${mode}" "${srcdir}" "${builddir}" "${args[@]}"
	fi
}

main "$@"
