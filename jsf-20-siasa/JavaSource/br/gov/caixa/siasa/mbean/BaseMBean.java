package br.gov.caixa.siasa.mbean;

import java.io.Serializable;

import javax.faces.application.FacesMessage;
import javax.faces.application.FacesMessage.Severity;
import javax.faces.context.FacesContext;

public class BaseMBean implements Serializable {

    private static final long serialVersionUID = -1138953502162336906L;

    protected void addErrorMessage(final String componentId, final String errorMessage) {
        addMessage(componentId, errorMessage, FacesMessage.SEVERITY_ERROR);
    }

    protected void addErrorMessage(final String errorMessage) {
        addMessage(null, errorMessage, FacesMessage.SEVERITY_ERROR);
    }

    protected void addInfoMessage(final String componentId, final String infoMessage) {
        addMessage(componentId, infoMessage, FacesMessage.SEVERITY_INFO);
    }

    protected void addInfoMessage(final String infoMessage) {
        addMessage(null, infoMessage, FacesMessage.SEVERITY_INFO);
    }

    protected void addMessage(final String componentId, final String message, final Severity severity) {
        FacesMessage facesMessage = new FacesMessage(message);
        facesMessage.setSeverity(severity);
        FacesContext.getCurrentInstance().addMessage(componentId, facesMessage);
    }
}
