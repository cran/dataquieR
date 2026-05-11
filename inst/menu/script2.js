function knowID(id) {
  return(all_ids.all_ids.includes(id) ||
         all_ids.all_ids.includes(decodeURI(id)))
}
