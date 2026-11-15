package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class Contacto {

	private String nombre;
	private List<Mensaje> mensajes;
	protected List<String> conversacion;

	public Contacto(String nombre) {
		setNombre(nombre);
		mensajes = new ArrayList<Mensaje>();
		conversacion = new ArrayList<String>();
		conversacion.add(0, "Conversacion");		
	}

	public String getNombre() {
		return nombre;
	}

	public void setNombre(String nombre) {
		this.nombre = nombre;
	}
	
	public void recibirMensaje(String mensaje) {
		mensajes.add(new MensajeRecibido(mensaje));
		conversacion.add(1, nombre + ": " + mensaje);		
	}

	public void enviarMensaje(String mensaje) {
		mensajes.add(new MensajeEnviado(mensaje));
		conversacion.add(1, "Yo: " + mensaje);			
	}

	public int cantidadTotalMensajesRecibidos() {
		int cantidadTotalMensajesRecibidos = 0;
		for (Mensaje mensaje: mensajes) {
			if (mensaje instanceof MensajeRecibido) {
				cantidadTotalMensajesRecibidos += 1;
			}
		}
		return cantidadTotalMensajesRecibidos;
	}
	
	public int cantidadTotalMensajesEnviados() {
		return mensajes.size() - cantidadTotalMensajesRecibidos();
	}

	public List<String> obtenerConversacion() {
		return conversacion;
	}

	public void borrarMensajes() {
		mensajes.clear();
		conversacion.clear();
		conversacion.add(0, "Conversacion");		
	}
}
