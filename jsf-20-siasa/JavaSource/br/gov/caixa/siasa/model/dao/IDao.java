package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.siasa.model.dto.CobolBook;

public interface IDao {
	
	CobolBook execute(CobolBook cb);
	
}
